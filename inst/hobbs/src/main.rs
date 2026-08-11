use std::env;
#[cfg(unix)]
use std::ffi::CStr;
use std::ffi::CString;
#[cfg(windows)]
use std::ffi::OsStr;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::os::raw::{c_char, c_int, c_void};
#[cfg(windows)]
use std::os::windows::ffi::OsStrExt;
use std::process;

#[cfg(all(unix, target_os = "linux"))]
#[link(name = "dl")]
extern "C" {}

#[cfg(unix)]
extern "C" {
    fn dlopen(filename: *const c_char, flag: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
    fn dlclose(handle: *mut c_void) -> c_int;
    fn dlerror() -> *const c_char;
}

#[cfg(unix)]
const RTLD_NOW: c_int = 2;

#[cfg(windows)]
#[link(name = "kernel32")]
extern "system" {
    #[link_name = "LoadLibraryW"]
    fn load_library_w(filename: *const u16) -> *mut c_void;
    #[link_name = "GetProcAddress"]
    fn get_proc_address(module: *mut c_void, symbol: *const c_char) -> *mut c_void;
    #[link_name = "FreeLibrary"]
    fn free_library(module: *mut c_void) -> c_int;
}
// Passing all adaptation state through the fused C ABI costs more than the
// Rust bookkeeping it replaces for very short vectors.  Benchmarks of the
// generated standard-normal target put the stable crossover at eight scalar
// coordinates, so shorter blocks use the lean compatibility sweep.
const FUSED_ADAPTIVE_SWEEP_MIN_LEN: usize = 8;
type LogpFn = unsafe extern "C" fn(*const f64, c_int) -> f64;
type LogpBatchFn = unsafe extern "C" fn(*const f64, c_int, c_int, *mut f64);
type InitFn = unsafe extern "C" fn(*const c_char) -> c_int;
type FreeFn = unsafe extern "C" fn();
type BlockFn = unsafe extern "C" fn(*const f64, c_int) -> f64;
type CacheInitFn = unsafe extern "C" fn(*const f64, c_int) -> c_int;
type CacheVoidFn = unsafe extern "C" fn();
type CacheUpdateFn = unsafe extern "C" fn(*const f64, c_int, f64);
type CacheReversibleFn = unsafe extern "C" fn() -> c_int;
type ContinuousSweepFn =
    unsafe extern "C" fn(*mut f64, *const f64, *const f64, *const f64, *mut u8, *mut c_int) -> f64;
type ContinuousAdaptiveSweepFn = unsafe extern "C" fn(
    *mut f64,
    *mut f64,
    *mut f64,
    *const f64,
    *const f64,
    *const f64,
    *mut u64,
    *mut u64,
    c_int,
    f64,
    f64,
    f64,
    f64,
    f64,
    f64,
    f64,
    *mut u64,
    *mut c_int,
) -> f64;
type ScalarCandidateFn = unsafe extern "C" fn(*mut f64, c_int, c_int, f64, f64) -> f64;
type ScalarAcceptFn = unsafe extern "C" fn();
type ScalarRejectFn = unsafe extern "C" fn(*mut f64, c_int, c_int, f64, f64);
type SwitchValueFn = unsafe extern "C" fn(*const f64, c_int) -> f64;
type SwitchApplyFn = unsafe extern "C" fn(*mut f64, c_int, f64, f64) -> c_int;

#[cfg(unix)]
unsafe fn open_library(path: &str) -> Result<*mut c_void, String> {
    let c_path = CString::new(path).map_err(|_| "library path contains NUL byte".to_string())?;
    let handle = dlopen(c_path.as_ptr(), RTLD_NOW);
    if handle.is_null() {
        Err(format!("dlopen failed: {}", last_dl_error()))
    } else {
        Ok(handle)
    }
}

#[cfg(windows)]
unsafe fn open_library(path: &str) -> Result<*mut c_void, String> {
    if path.encode_utf16().any(|unit| unit == 0) {
        return Err("library path contains NUL byte".to_string());
    }
    let wide_path: Vec<u16> = OsStr::new(path)
        .encode_wide()
        .chain(std::iter::once(0))
        .collect();
    let handle = load_library_w(wide_path.as_ptr());
    if handle.is_null() {
        Err(format!(
            "LoadLibraryW failed: {}",
            std::io::Error::last_os_error()
        ))
    } else {
        Ok(handle)
    }
}

#[cfg(unix)]
unsafe fn close_library(handle: *mut c_void) {
    let _ = dlclose(handle);
}

#[cfg(windows)]
unsafe fn close_library(handle: *mut c_void) {
    let _ = free_library(handle);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EvalMode {
    Auto,
    Scalar,
    Batch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutputFormat {
    Binary,
    Csv,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SaveMode {
    Chain,
    Mean,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UpdateMode {
    Global,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CovarianceMode {
    Auto,
    Full,
    Diagonal,
    Off,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ShapeAdaptationMode {
    ConditionalFull,
    MarginalDiagonal,
    Off,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueKind {
    Continuous,
    Discrete,
}

impl ValueKind {
    fn as_str(&self) -> &'static str {
        match self {
            ValueKind::Continuous => "continuous",
            ValueKind::Discrete => "discrete",
        }
    }
}

#[derive(Debug, Clone)]
struct BlockSpec {
    name: String,
    offset: usize,
    len: usize,
    value_kind: ValueKind,
    lower: i64,
    upper: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SaveRange {
    offset: usize,
    len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DeclaredSwitchSpec {
    position_j: usize,
    position_k: usize,
}

struct RuntimeBlock {
    name: String,
    offset: usize,
    len: usize,
    value_kind: ValueKind,
    lower: i64,
    upper: i64,
    f: BlockFn,
    continuous_adaptive_sweep: Option<ContinuousAdaptiveSweepFn>,
    continuous_sweep: Option<ContinuousSweepFn>,
    scalar_candidate: Option<ScalarCandidateFn>,
    scalar_accept: Option<ScalarAcceptFn>,
    scalar_reject: Option<ScalarRejectFn>,
    cache_update: Option<CacheUpdateFn>,
    cache_undo: Option<CacheUpdateFn>,
    cache_update_reversible: bool,
}

struct PosteriorLib {
    handle: *mut c_void,
    logp: Option<LogpFn>,
    logp_batch: Option<LogpBatchFn>,
    init: Option<InitFn>,
    free: Option<FreeFn>,
    cache_init: Option<CacheInitFn>,
    cache_free: Option<CacheVoidFn>,
    cache_snapshot: Option<CacheVoidFn>,
    cache_restore: Option<CacheVoidFn>,
    switch_values: Vec<SwitchValueFn>,
    switch_applies: Vec<SwitchApplyFn>,
    cache_initialized: bool,
    initialized: bool,
    mode: EvalMode,
    one_out: [f64; 1],
}

impl PosteriorLib {
    fn open(
        path: &str,
        requested_mode: EvalMode,
        allow_missing_logp: bool,
        switch_derived_count: usize,
    ) -> Result<Self, String> {
        unsafe {
            let handle = open_library(path)?;

            let logp_ptr = optional_symbol(handle, "posterior_logp");
            let batch_ptr = optional_symbol(handle, "posterior_logp_batch");
            let init_ptr = optional_symbol(handle, "posterior_init");
            let free_ptr = optional_symbol(handle, "posterior_free");
            let cache_init_ptr = optional_symbol(handle, "hobbs_cache_init");
            let cache_free_ptr = optional_symbol(handle, "hobbs_cache_free");
            let cache_snapshot_ptr = optional_symbol(handle, "hobbs_cache_snapshot");
            let cache_restore_ptr = optional_symbol(handle, "hobbs_cache_restore");
            let mut switch_values = Vec::with_capacity(switch_derived_count);
            let mut switch_applies = Vec::with_capacity(switch_derived_count);
            for index in 0..switch_derived_count {
                let value_symbol = format!("hobbs_switch_value_{}", index);
                let apply_symbol = format!("hobbs_switch_apply_{}", index);
                let Some(value_ptr) = optional_symbol(handle, &value_symbol) else {
                    close_library(handle);
                    return Err(format!(
                        "derived switch symbol {} was not found",
                        value_symbol
                    ));
                };
                let Some(apply_ptr) = optional_symbol(handle, &apply_symbol) else {
                    close_library(handle);
                    return Err(format!(
                        "derived switch symbol {} was not found",
                        apply_symbol
                    ));
                };
                switch_values.push(std::mem::transmute::<*mut c_void, SwitchValueFn>(value_ptr));
                switch_applies.push(std::mem::transmute::<*mut c_void, SwitchApplyFn>(apply_ptr));
            }

            if logp_ptr.is_none() && batch_ptr.is_none() && !allow_missing_logp {
                close_library(handle);
                return Err(
                    "library must export posterior_logp or posterior_logp_batch".to_string()
                );
            }

            let logp = logp_ptr.map(|p| std::mem::transmute::<*mut c_void, LogpFn>(p));
            let logp_batch = batch_ptr.map(|p| std::mem::transmute::<*mut c_void, LogpBatchFn>(p));
            let init = init_ptr.map(|p| std::mem::transmute::<*mut c_void, InitFn>(p));
            let free = free_ptr.map(|p| std::mem::transmute::<*mut c_void, FreeFn>(p));
            let cache_init =
                cache_init_ptr.map(|p| std::mem::transmute::<*mut c_void, CacheInitFn>(p));
            let cache_free =
                cache_free_ptr.map(|p| std::mem::transmute::<*mut c_void, CacheVoidFn>(p));
            let cache_snapshot =
                cache_snapshot_ptr.map(|p| std::mem::transmute::<*mut c_void, CacheVoidFn>(p));
            let cache_restore =
                cache_restore_ptr.map(|p| std::mem::transmute::<*mut c_void, CacheVoidFn>(p));

            let mode = match requested_mode {
                EvalMode::Scalar => {
                    if logp.is_none() {
                        if allow_missing_logp {
                            EvalMode::Scalar
                        } else {
                            close_library(handle);
                            return Err("--eval scalar requested but posterior_logp was not found"
                                .to_string());
                        }
                    } else {
                        EvalMode::Scalar
                    }
                }
                EvalMode::Batch => {
                    if logp_batch.is_none() {
                        if allow_missing_logp {
                            EvalMode::Scalar
                        } else {
                            close_library(handle);
                            return Err(
                                "--eval batch requested but posterior_logp_batch was not found"
                                    .to_string(),
                            );
                        }
                    } else {
                        EvalMode::Batch
                    }
                }
                EvalMode::Auto => {
                    // Single-chain MH needs one logp at a time. Scalar is normally fastest for n_batch=1.
                    if logp.is_some() {
                        EvalMode::Scalar
                    } else if logp_batch.is_some() {
                        EvalMode::Batch
                    } else {
                        EvalMode::Scalar
                    }
                }
            };

            Ok(Self {
                handle,
                logp,
                logp_batch,
                init,
                free,
                cache_init,
                cache_free,
                cache_snapshot,
                cache_restore,
                switch_values,
                switch_applies,
                cache_initialized: false,
                initialized: false,
                mode,
                one_out: [0.0],
            })
        }
    }

    fn init_with_data(&mut self, data_path: Option<&str>) -> Result<(), String> {
        if let Some(init_fn) = self.init {
            let path = data_path.ok_or_else(|| {
                "posterior exports posterior_init but --data was not supplied".to_string()
            })?;
            let c_path =
                CString::new(path).map_err(|_| "data path contains NUL byte".to_string())?;
            let rc = unsafe { init_fn(c_path.as_ptr()) };
            if rc != 0 {
                return Err(format!("posterior_init failed with status {}", rc));
            }
            self.initialized = true;
        } else if data_path.is_some() {
            return Err(
                "--data was supplied, but library does not export posterior_init".to_string(),
            );
        }
        Ok(())
    }

    fn has_init(&self) -> bool {
        self.init.is_some()
    }
    fn has_free(&self) -> bool {
        self.free.is_some()
    }
    fn has_cache(&self) -> bool {
        self.cache_init.is_some()
    }
    fn has_cache_snapshot_restore(&self) -> bool {
        self.cache_snapshot.is_some() && self.cache_restore.is_some()
    }

    fn init_cache(&mut self, theta: &[f64]) -> Result<(), String> {
        if let Some(f) = self.cache_init {
            let rc = unsafe { f(theta.as_ptr(), theta.len() as c_int) };
            if rc != 0 {
                return Err(format!("hobbs_cache_init failed with status {}", rc));
            }
            self.cache_initialized = true;
        }
        Ok(())
    }

    #[inline(always)]
    fn cache_snapshot(&self) {
        if let Some(f) = self.cache_snapshot {
            unsafe {
                f();
            }
        }
    }
    #[inline(always)]
    fn cache_restore(&self) {
        if let Some(f) = self.cache_restore {
            unsafe {
                f();
            }
        }
    }

    #[inline(always)]
    fn switch_coordinate_value(&self, position: usize, theta: &[f64]) -> Option<f64> {
        let value = if position < theta.len() {
            theta[position]
        } else {
            let derived_index = position.checked_sub(theta.len())?;
            let value_fn = *self.switch_values.get(derived_index)?;
            unsafe { value_fn(theta.as_ptr(), theta.len() as c_int) }
        };
        if value.is_finite() { Some(value) } else { None }
    }

    #[inline(always)]
    fn assign_switch_coordinate(
        &self,
        position: usize,
        current_value: f64,
        proposed_value: f64,
        theta: &mut [f64],
    ) -> bool {
        if !proposed_value.is_finite() {
            return false;
        }
        if position < theta.len() {
            theta[position] = proposed_value;
            true
        } else {
            let Some(derived_index) = position.checked_sub(theta.len()) else {
                return false;
            };
            let Some(apply_fn) = self.switch_applies.get(derived_index).copied() else {
                return false;
            };
            unsafe {
                apply_fn(
                    theta.as_mut_ptr(),
                    theta.len() as c_int,
                    current_value,
                    proposed_value,
                ) == 0
            }
        }
    }

    fn switch_derived_count(&self) -> usize {
        self.switch_values.len()
    }

    #[inline(always)]
    fn logp(&mut self, theta: &[f64]) -> f64 {
        match self.mode {
            EvalMode::Scalar | EvalMode::Auto => unsafe {
                (self.logp.unwrap_unchecked())(theta.as_ptr(), theta.len() as c_int)
            },
            EvalMode::Batch => unsafe {
                (self.logp_batch.unwrap_unchecked())(
                    theta.as_ptr(),
                    theta.len() as c_int,
                    1,
                    self.one_out.as_mut_ptr(),
                );
                self.one_out[0]
            },
        }
    }

    fn load_blocks(&self, specs: &[BlockSpec]) -> Result<Vec<RuntimeBlock>, String> {
        let mut out = Vec::with_capacity(specs.len());
        for b in specs {
            let sym = format!("hobbs_block_{}", b.name);
            let ptr = unsafe { optional_symbol(self.handle, &sym) };
            let Some(ptr) = ptr else {
                return Err(format!(
                    "--update block requested, but symbol {} was not found",
                    sym
                ));
            };
            let f = unsafe { std::mem::transmute::<*mut c_void, BlockFn>(ptr) };
            let adaptive_sweep_sym = format!("hobbs_sweep_adapt_{}", b.name);
            let continuous_adaptive_sweep = unsafe {
                optional_symbol(self.handle, &adaptive_sweep_sym)
            }
            .map(|p| unsafe { std::mem::transmute::<*mut c_void, ContinuousAdaptiveSweepFn>(p) });
            let sweep_sym = format!("hobbs_sweep_{}", b.name);
            let continuous_sweep = unsafe { optional_symbol(self.handle, &sweep_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ContinuousSweepFn>(p) });
            let candidate_sym = format!("hobbs_scalar_candidate_{}", b.name);
            let scalar_candidate = unsafe { optional_symbol(self.handle, &candidate_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ScalarCandidateFn>(p) });
            let accept_sym = format!("hobbs_scalar_accept_{}", b.name);
            let scalar_accept = unsafe { optional_symbol(self.handle, &accept_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ScalarAcceptFn>(p) });
            let reject_sym = format!("hobbs_scalar_reject_{}", b.name);
            let scalar_reject = unsafe { optional_symbol(self.handle, &reject_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ScalarRejectFn>(p) });
            let update_sym = format!("hobbs_cache_update_{}", b.name);
            let cache_update = unsafe { optional_symbol(self.handle, &update_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, CacheUpdateFn>(p) });
            let undo_sym = format!("hobbs_cache_undo_{}", b.name);
            let cache_undo = unsafe { optional_symbol(self.handle, &undo_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, CacheUpdateFn>(p) });
            let reversible_sym = format!("hobbs_cache_update_reversible_{}", b.name);
            let cache_update_reversible = unsafe { optional_symbol(self.handle, &reversible_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, CacheReversibleFn>(p)() != 0 })
                .unwrap_or(false);
            out.push(RuntimeBlock {
                name: b.name.clone(),
                offset: b.offset,
                len: b.len,
                value_kind: b.value_kind,
                lower: b.lower,
                upper: b.upper,
                f,
                continuous_adaptive_sweep,
                continuous_sweep,
                scalar_candidate,
                scalar_accept,
                scalar_reject,
                cache_update,
                cache_undo,
                cache_update_reversible,
            });
        }
        Ok(out)
    }

    fn has_scalar(&self) -> bool {
        self.logp.is_some()
    }
    fn has_batch(&self) -> bool {
        self.logp_batch.is_some()
    }
    fn active_mode_name(&self) -> &'static str {
        match self.mode {
            EvalMode::Auto => "auto",
            EvalMode::Scalar => "scalar",
            EvalMode::Batch => "batch",
        }
    }
}

impl Drop for PosteriorLib {
    fn drop(&mut self) {
        unsafe {
            if self.cache_initialized {
                if let Some(cache_free_fn) = self.cache_free {
                    cache_free_fn();
                }
            }
            if self.initialized {
                if let Some(free_fn) = self.free {
                    free_fn();
                }
            }
            close_library(self.handle);
        }
    }
}

#[cfg(unix)]
unsafe fn optional_symbol(handle: *mut c_void, name: &str) -> Option<*mut c_void> {
    let _ = dlerror();
    let cname = CString::new(name).unwrap();
    let ptr = dlsym(handle, cname.as_ptr());
    if ptr.is_null() {
        None
    } else {
        Some(ptr)
    }
}

#[cfg(windows)]
unsafe fn optional_symbol(handle: *mut c_void, name: &str) -> Option<*mut c_void> {
    let cname = CString::new(name).unwrap();
    let ptr = get_proc_address(handle, cname.as_ptr());
    if ptr.is_null() {
        None
    } else {
        Some(ptr)
    }
}

#[cfg(unix)]
fn last_dl_error() -> String {
    unsafe {
        let err = dlerror();
        if err.is_null() {
            "unknown dynamic loader error".to_string()
        } else {
            CStr::from_ptr(err).to_string_lossy().into_owned()
        }
    }
}

#[derive(Debug, Clone)]
struct Config {
    lib: String,
    data: Option<String>,
    dim: usize,
    samples: usize,
    burnin: usize,
    thin: usize,
    seed: u64,
    step: f64,
    adapt_until: Option<usize>,
    target_accept: f64,
    target_accept_set: bool,
    covariance_mode: CovarianceMode,
    covariance_max_dim: usize,
    switch_enabled: bool,
    switch_threshold: f64,
    switch_max_dim: usize,
    switch_derived_count: usize,
    declared_switches: Vec<DeclaredSwitchSpec>,
    adapt_diagnostics_out: Option<String>,
    adapt_covariance_out: Option<String>,
    switch_diagnostics_out: Option<String>,
    out: Option<String>,
    mean_out: Option<String>,
    mean_ranges: Vec<SaveRange>,
    out_format: OutputFormat,
    save_mode: SaveMode,
    update_mode: UpdateMode,
    blocks: Vec<BlockSpec>,
    eval_mode: EvalMode,
    quiet: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            lib: String::new(),
            data: None,
            dim: 2,
            samples: 1000,
            burnin: 500,
            thin: 1,
            seed: 0x1234_5678_9abc_def0,
            step: 0.25,
            adapt_until: None,
            target_accept: 0.44,
            target_accept_set: false,
            covariance_mode: CovarianceMode::Auto,
            covariance_max_dim: 128,
            switch_enabled: false,
            switch_threshold: 0.8,
            switch_max_dim: 128,
            switch_derived_count: 0,
            declared_switches: Vec::new(),
            adapt_diagnostics_out: None,
            adapt_covariance_out: None,
            switch_diagnostics_out: None,
            out: Some("chain.bin".to_string()),
            mean_out: None,
            mean_ranges: Vec::new(),
            out_format: OutputFormat::Binary,
            save_mode: SaveMode::Chain,
            update_mode: UpdateMode::Global,
            blocks: Vec::new(),
            eval_mode: EvalMode::Auto,
            quiet: false,
        }
    }
}

fn parse_args() -> Config {
    let mut cfg = Config::default();
    let args: Vec<String> = env::args().collect();
    let mut i = 1;
    while i < args.len() {
        let key = args[i].as_str();
        if key == "--help" || key == "-h" {
            print_help_and_exit();
        }
        match key {
            "--no-output" => {
                cfg.out = None;
                i += 1;
                continue;
            }
            "--quiet" => {
                cfg.quiet = true;
                i += 1;
                continue;
            }
            "--switch" => {
                cfg.switch_enabled = true;
                i += 1;
                continue;
            }
            _ => {}
        }
        if i + 1 >= args.len() {
            eprintln!("missing value for {}", key);
            process::exit(2);
        }
        let val = &args[i + 1];
        match key {
            "--lib" => cfg.lib = val.clone(),
            "--data" => cfg.data = Some(val.clone()),
            "--dim" => cfg.dim = parse_positive(val, "--dim"),
            "--samples" => cfg.samples = parse_positive(val, "--samples"),
            "--burnin" | "--warmups" => cfg.burnin = parse_usize(val, key),
            "--thin" => cfg.thin = parse_positive(val, "--thin"),
            "--seed" => cfg.seed = parse_u64(val, "--seed"),
            "--step" => cfg.step = parse_f64(val, "--step"),
            "--adapt-every" => {
                // Accepted only for backward CLI compatibility. Geometry is
                // accumulated over one fixed window and fitted once.
                let _ = parse_positive(val, "--adapt-every");
            }
            "--adapt-until" => cfg.adapt_until = Some(parse_usize(val, "--adapt-until")),
            "--target-accept" => {
                cfg.target_accept = parse_f64(val, "--target-accept");
                cfg.target_accept_set = true;
            }
            "--adapt-covariance" | "--covariance" => {
                cfg.covariance_mode = parse_covariance_mode(val)
            }
            "--adapt-covariance-max-dim" | "--covariance-max-dim" => {
                cfg.covariance_max_dim = parse_positive(val, "--adapt-covariance-max-dim")
            }
            "--switch-threshold" => {
                cfg.switch_threshold = parse_f64(val, "--switch-threshold")
            }
            "--switch-max-dim" => {
                cfg.switch_max_dim = parse_positive(val, "--switch-max-dim")
            }
            "--switch-derived-count" => {
                cfg.switch_derived_count = parse_usize(val, "--switch-derived-count")
            }
            "--switch-pair" => cfg.declared_switches.push(parse_declared_switch_spec(val)),
            "--switch-pairs-file" => {
                cfg.declared_switches.extend(read_declared_switch_specs(val));
            }
            "--adapt-diagnostics-out" => cfg.adapt_diagnostics_out = Some(val.clone()),
            "--adapt-cov-out" | "--adapt-covariance-out" => {
                cfg.adapt_covariance_out = Some(val.clone())
            }
            "--switch-diagnostics-out" => {
                cfg.switch_diagnostics_out = Some(val.clone())
            }
            "--out" => {
                cfg.out = Some(val.clone());
                if val.ends_with(".csv") {
                    cfg.out_format = OutputFormat::Csv;
                } else if val.ends_with(".bin") {
                    cfg.out_format = OutputFormat::Binary;
                }
            }
            "--mean-out" => cfg.mean_out = Some(val.clone()),
            "--mean-range" => cfg.mean_ranges.push(parse_save_range(val)),
            "--format" => cfg.out_format = parse_output_format(val),
            "--save" => cfg.save_mode = parse_save_mode(val),
            "--update" => cfg.update_mode = parse_update_mode(val),
            "--block" => cfg.blocks.push(parse_block_spec(val)),
            "--eval" => cfg.eval_mode = parse_eval_mode(val),
            _ => {
                eprintln!("unknown argument: {}", key);
                eprintln!("use --help for usage");
                process::exit(2);
            }
        }
        i += 2;
    }
    if cfg.lib.is_empty() {
        eprintln!("error: --lib path/to/posterior.so is required\n");
        print_help_and_exit();
    }
    if !(cfg.step.is_finite() && cfg.step > 0.0) {
        eprintln!("--step must be positive and finite");
        process::exit(2);
    }
    if !cfg.target_accept_set {
        // Every continuous transition is a one-dimensional random walk,
        // including the full-posterior fallback mode.
        cfg.target_accept = 0.44;
    }
    if !(cfg.target_accept > 0.0 && cfg.target_accept < 1.0) {
        eprintln!("--target-accept must be between 0 and 1");
        process::exit(2);
    }
    if !(cfg.switch_threshold > 0.0 && cfg.switch_threshold < 1.0) {
        eprintln!("--switch-threshold must be between 0 and 1");
        process::exit(2);
    }
    if !cfg.declared_switches.is_empty() {
        cfg.switch_enabled = true;
    }
    let switch_coordinate_count = cfg
        .dim
        .checked_add(cfg.switch_derived_count)
        .unwrap_or_else(|| {
            eprintln!("--dim plus --switch-derived-count is too large");
            process::exit(2);
        });
    for pair in &cfg.declared_switches {
        if pair.position_j >= switch_coordinate_count || pair.position_k >= switch_coordinate_count {
            eprintln!(
                "--switch-pair {}:{} exceeds the {} available ordinary-plus-derived switch coordinates",
                pair.position_j, pair.position_k, switch_coordinate_count
            );
            process::exit(2);
        }
        if pair.position_j == pair.position_k {
            eprintln!("--switch-pair cannot pair a coordinate with itself");
            process::exit(2);
        }
    }
    if cfg.adapt_until.is_none() {
        cfg.adapt_until = Some(cfg.burnin);
    }
    if cfg.switch_enabled && cfg.adapt_until.unwrap_or(0) < 20 {
        eprintln!("--switch requires at least 20 adaptation/warmup sweeps");
        process::exit(2);
    }
    if cfg.switch_enabled && cfg.adapt_until.unwrap_or(0) > cfg.burnin {
        eprintln!("--switch requires --adapt-until to be no greater than --burnin/--warmups so pair learning freezes before retained sampling");
        process::exit(2);
    }
    if cfg.update_mode == UpdateMode::Block && cfg.blocks.is_empty() {
        eprintln!("--update block requires at least one --block name:offset:len");
        process::exit(2);
    }
    if cfg.dim > c_int::MAX as usize {
        eprintln!("--dim must be at most {} for the C model ABI", c_int::MAX);
        process::exit(2);
    }
    for block in &cfg.blocks {
        if block.len > c_int::MAX as usize {
            eprintln!(
                "--block {} length must be at most {} for scalar C indexing",
                block.name,
                c_int::MAX
            );
            process::exit(2);
        }
    }
    normalize_save_ranges(&mut cfg.mean_ranges, cfg.dim);
    if cfg.save_mode == SaveMode::Mean && !cfg.mean_ranges.is_empty() {
        eprintln!("--mean-range cannot be combined with global --save mean");
        process::exit(2);
    }
    if cfg.out.is_none() {
        cfg.mean_out = None;
    } else if cfg.mean_ranges.is_empty() {
        if cfg.mean_out.is_some() {
            eprintln!("--mean-out requires at least one --mean-range");
            process::exit(2);
        }
    } else {
        let mean_dim: usize = cfg.mean_ranges.iter().map(|range| range.len).sum();
        if cfg.mean_out.is_none() {
            eprintln!("declaration-level means require --mean-out PATH");
            process::exit(2);
        }
        if mean_dim < cfg.dim && cfg.mean_out.as_deref() == cfg.out.as_deref() {
            eprintln!(
                "--mean-out must differ from --out when any parameters retain full-chain output"
            );
            process::exit(2);
        }
    }
    cfg
}

fn parse_declared_switch_spec(s: &str) -> DeclaredSwitchSpec {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 2 {
        eprintln!("--switch-pair must have form zero_based_position_j:zero_based_position_k");
        process::exit(2);
    }
    DeclaredSwitchSpec {
        position_j: parse_usize(parts[0], "--switch-pair position_j"),
        position_k: parse_usize(parts[1], "--switch-pair position_k"),
    }
}

fn read_declared_switch_specs(path: &str) -> Vec<DeclaredSwitchSpec> {
    let file = File::open(path).unwrap_or_else(|error| {
        eprintln!("could not open --switch-pairs-file {}: {}", path, error);
        process::exit(2);
    });
    let reader = BufReader::new(file);
    let mut pairs = Vec::new();
    for (line_index, line) in reader.lines().enumerate() {
        let line = line.unwrap_or_else(|error| {
            eprintln!(
                "could not read --switch-pairs-file {} line {}: {}",
                path,
                line_index + 1,
                error
            );
            process::exit(2);
        });
        let value = line.split('#').next().unwrap_or("").trim();
        if value.is_empty() {
            continue;
        }
        pairs.push(parse_declared_switch_spec(value));
    }
    pairs
}

fn parse_update_mode(s: &str) -> UpdateMode {
    match s {
        "global" | "dense" => UpdateMode::Global,
        "block" | "local" => UpdateMode::Block,
        _ => {
            eprintln!("--update must be one of: global, block");
            process::exit(2);
        }
    }
}

fn parse_block_spec(s: &str) -> BlockSpec {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 3 && parts.len() != 4 && parts.len() != 7 {
        eprintln!("--block must have form name:offset:len[:indexed] or name:offset:len:indexed:continuous|discrete:lower:upper");
        process::exit(2);
    }
    let name = parts[0].to_string();
    let offset = parse_usize(parts[1], "--block offset");
    let len = parse_positive(parts[2], "--block len");
    if parts.len() >= 4 {
        match parts[3] {
            "indexed" | "scalar" | "element" => {}
            "group" | "joint" => {
                eprintln!("joint block proposals are not supported; use block name(j) for scalar Metropolis-within-Gibbs updates");
                process::exit(2);
            }
            _ => {
                eprintln!("--block kind must be indexed");
                process::exit(2);
            }
        }
    }
    let (value_kind, lower, upper) = if parts.len() == 7 {
        let vk = match parts[4] {
            "continuous" | "real" => ValueKind::Continuous,
            "discrete" | "integer" | "int" => ValueKind::Discrete,
            _ => {
                eprintln!("--block value kind must be continuous or discrete");
                process::exit(2);
            }
        };
        let lo = parse_i64(parts[5], "--block lower");
        let hi = parse_i64(parts[6], "--block upper");
        if vk == ValueKind::Discrete && hi < lo {
            eprintln!("--block discrete upper bound must be >= lower bound");
            process::exit(2);
        }
        (vk, lo, hi)
    } else {
        (ValueKind::Continuous, 0, 0)
    };
    BlockSpec {
        name,
        offset,
        len,
        value_kind,
        lower,
        upper,
    }
}

fn parse_save_range(s: &str) -> SaveRange {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 2 {
        eprintln!("--mean-range must have form offset:len");
        process::exit(2);
    }
    SaveRange {
        offset: parse_usize(parts[0], "--mean-range offset"),
        len: parse_positive(parts[1], "--mean-range len"),
    }
}

fn normalize_save_ranges(ranges: &mut Vec<SaveRange>, dim: usize) {
    ranges.sort_unstable_by_key(|range| range.offset);
    let mut merged: Vec<SaveRange> = Vec::with_capacity(ranges.len());
    for range in ranges.iter().copied() {
        let end = range.offset.checked_add(range.len).unwrap_or_else(|| {
            eprintln!("--mean-range offset/length overflow");
            process::exit(2);
        });
        if end > dim {
            eprintln!(
                "--mean-range {}:{} exceeds --dim {}",
                range.offset, range.len, dim
            );
            process::exit(2);
        }
        if let Some(last) = merged.last_mut() {
            let last_end = last.offset + last.len;
            if range.offset < last_end {
                eprintln!("--mean-range entries must not overlap");
                process::exit(2);
            }
            if range.offset == last_end {
                last.len += range.len;
                continue;
            }
        }
        merged.push(range);
    }
    *ranges = merged;
}

fn complement_save_ranges(dim: usize, excluded: &[SaveRange]) -> Vec<SaveRange> {
    let mut included = Vec::with_capacity(excluded.len() + 1);
    let mut cursor = 0usize;
    for range in excluded {
        if cursor < range.offset {
            included.push(SaveRange {
                offset: cursor,
                len: range.offset - cursor,
            });
        }
        cursor = range.offset + range.len;
    }
    if cursor < dim {
        included.push(SaveRange {
            offset: cursor,
            len: dim - cursor,
        });
    }
    included
}

fn parse_covariance_mode(s: &str) -> CovarianceMode {
    match s {
        "auto" => CovarianceMode::Auto,
        "full" | "dense" => CovarianceMode::Full,
        "diagonal" | "diag" => CovarianceMode::Diagonal,
        "off" | "none" => CovarianceMode::Off,
        _ => {
            eprintln!("--adapt-covariance must be one of: auto, full, diagonal, off");
            process::exit(2);
        }
    }
}

fn parse_eval_mode(s: &str) -> EvalMode {
    match s {
        "auto" => EvalMode::Auto,
        "scalar" => EvalMode::Scalar,
        "batch" => EvalMode::Batch,
        _ => {
            eprintln!("--eval must be one of: auto, scalar, batch");
            process::exit(2);
        }
    }
}

fn parse_save_mode(s: &str) -> SaveMode {
    match s {
        "chain" | "draws" | "samples" => SaveMode::Chain,
        "mean" | "mean-only" | "posterior-mean" => SaveMode::Mean,
        _ => {
            eprintln!("--save must be one of: chain, mean");
            process::exit(2);
        }
    }
}

fn parse_output_format(s: &str) -> OutputFormat {
    match s {
        "bin" | "binary" => OutputFormat::Binary,
        "csv" => OutputFormat::Csv,
        _ => {
            eprintln!("--format must be one of: bin, binary, csv");
            process::exit(2);
        }
    }
}

fn parse_usize(s: &str, name: &str) -> usize {
    s.parse::<usize>().unwrap_or_else(|_| {
        eprintln!("{} must be an unsigned integer", name);
        process::exit(2);
    })
}
fn parse_i64(s: &str, name: &str) -> i64 {
    s.parse::<i64>().unwrap_or_else(|_| {
        eprintln!("{} must be an integer", name);
        process::exit(2);
    })
}
fn parse_positive(s: &str, name: &str) -> usize {
    let x = parse_usize(s, name);
    if x == 0 {
        eprintln!("{} must be > 0", name);
        process::exit(2);
    }
    x
}
fn parse_u64(s: &str, name: &str) -> u64 {
    s.parse::<u64>().unwrap_or_else(|_| {
        eprintln!("{} must be an unsigned integer", name);
        process::exit(2);
    })
}
fn parse_f64(s: &str, name: &str) -> f64 {
    s.parse::<f64>().unwrap_or_else(|_| {
        eprintln!("{} must be a number", name);
        process::exit(2);
    })
}

fn print_help_and_exit() -> ! {
    println!(
        r#"hobbs: single-chain adaptive scalar Metropolis-within-Gibbs

Required:
  --lib PATH              Shared library exporting posterior_logp and/or posterior_logp_batch
  --data PATH             Optional data file passed to posterior_init(path)

Defaults:
  --dim 2
  --samples 1000          Saved samples after burn-in
  --burnin 500            Warmup/burn-in sweeps (`--warmups` is an alias)
  --thin 1
  --step 0.25             Initial proposal step size
  --adapt-every 25        Retained for compatibility; covariance geometry is fit once
  --adapt-until burnin    Last iteration allowed to adapt; default equals --burnin
  --target-accept 0.44    Per-coordinate Robbins-Monro acceptance target
  --seed 1311768467463790320
  --out chain.bin         Binary output is the default
  --format bin            bin/binary or csv for chain output
  --save chain            chain or mean. mean writes only post-burn-in means
  --mean-range off:len    Parameter range averaged instead of written per draw; repeatable
  --mean-out PATH         One-row standard binary for declaration-level means
  --update global         full-target scalar MWG or indexed block/local MWG
  --block name:off:len[:indexed[:type:lo:hi]] Scalar block metadata; type may be discrete
  --adapt-covariance auto  full conditional-variance, diagonal, or off
  --adapt-covariance-max-dim 128  auto uses full covariance through this dimension
  --switch                Learn and apply deterministic correlation switches
  --switch-threshold 0.8  Minimum absolute correlation for a greedy disjoint pair
  --switch-max-dim 128    Maximum continuous dimension for automatic dense greedy discovery
  --switch-derived-count N  Number of generated derived switch coordinates in the model library
  --switch-pair j:k       Explicit zero-based ordinary/derived pair; repeatable and may overlap
  --switch-pairs-file PATH  File containing one zero-based j:k pair per line
  --switch-diagnostics-out PATH  Frozen pair and acceptance diagnostics CSV
  --adapt-diagnostics-out PATH  Per-coordinate adaptation diagnostics CSV
  --adapt-cov-out PATH     Learned covariance/correlation diagnostics CSV
  --eval auto             auto, scalar, or batch

Output:
  --no-output             Disable writing output; best for raw speed benchmarks
  --quiet                 Suppress status lines except errors

C ABI:
  int posterior_init(const char* data_path);      // optional, for data-loading models
  double posterior_logp(const double* theta, int dim);
  void posterior_logp_batch(const double* theta, int dim, int n_batch, double* out);
  void posterior_free(void);                      // optional

Examples:
  ./target/release/hobbs --lib ./banana.so --dim 2
  ./target/release/hobbs --lib ./banana.so --dim 2 --no-output
  ./target/release/hobbs --lib ./banana.so --dim 2 --out chain.csv --format csv
"#
    );
    process::exit(0);
}

const NORMAL_ZIGGURAT_LAYERS: usize = 128;
const NORMAL_ZIGGURAT_MASK: u32 = (NORMAL_ZIGGURAT_LAYERS as u32) - 1;
const NORMAL_ZIGGURAT_R: f64 = 3.442_619_855_899;

struct Xoshiro256StarStar {
    s: [u64; 4],
    normal_k: [u32; NORMAL_ZIGGURAT_LAYERS],
    normal_w: [f64; NORMAL_ZIGGURAT_LAYERS],
    normal_f: [f64; NORMAL_ZIGGURAT_LAYERS],
}

impl Xoshiro256StarStar {
    fn new(seed: u64) -> Self {
        let mut x = seed;
        let mut s = [0u64; 4];
        for item in &mut s {
            *item = splitmix64(&mut x);
        }
        let (normal_k, normal_w, normal_f) = build_normal_ziggurat_tables();
        Self {
            s,
            normal_k,
            normal_w,
            normal_f,
        }
    }

    #[inline(always)]
    fn next_u64(&mut self) -> u64 {
        let result = self.s[1].wrapping_mul(5).rotate_left(7).wrapping_mul(9);
        let t = self.s[1] << 17;
        self.s[2] ^= self.s[0];
        self.s[3] ^= self.s[1];
        self.s[1] ^= self.s[2];
        self.s[0] ^= self.s[3];
        self.s[2] ^= t;
        self.s[3] = self.s[3].rotate_left(45);
        result
    }

    #[inline(always)]
    fn uniform_open01(&mut self) -> f64 {
        let x = self.next_u64() >> 11;
        ((x as f64) + 0.5) * (1.0 / ((1u64 << 53) as f64))
    }

    #[inline(always)]
    fn normal(&mut self) -> f64 {
        // 128-layer Marsaglia-Tsang ziggurat. Roughly 99% of proposals take
        // only one integer draw, one table comparison, and one multiply. The
        // logarithmic tail/acceptance path is rare, which matters for cheap
        // scalar targets where random-number generation is measurable.
        let mut hz = (self.next_u64() >> 32) as u32 as i32;
        let mut layer = ((hz as u32) & NORMAL_ZIGGURAT_MASK) as usize;
        loop {
            if hz.unsigned_abs() < self.normal_k[layer] {
                return hz as f64 * self.normal_w[layer];
            }

            let value = hz as f64 * self.normal_w[layer];
            if layer == 0 {
                loop {
                    let tail = -self.uniform_open01().ln() / NORMAL_ZIGGURAT_R;
                    let height = -self.uniform_open01().ln();
                    if height + height >= tail * tail {
                        return if hz > 0 {
                            NORMAL_ZIGGURAT_R + tail
                        } else {
                            -NORMAL_ZIGGURAT_R - tail
                        };
                    }
                }
            }

            let lower = self.normal_f[layer];
            let upper = self.normal_f[layer - 1];
            if lower + self.uniform_open01() * (upper - lower) < (-0.5 * value * value).exp() {
                return value;
            }

            hz = (self.next_u64() >> 32) as u32 as i32;
            layer = ((hz as u32) & NORMAL_ZIGGURAT_MASK) as usize;
        }
    }
}

fn build_normal_ziggurat_tables() -> (
    [u32; NORMAL_ZIGGURAT_LAYERS],
    [f64; NORMAL_ZIGGURAT_LAYERS],
    [f64; NORMAL_ZIGGURAT_LAYERS],
) {
    // Tables are generated once per chain to avoid embedding architecture-
    // dependent binary literals. This setup cost is negligible relative to a
    // sampler run and keeps the implementation self-contained.
    let mut k = [0u32; NORMAL_ZIGGURAT_LAYERS];
    let mut w = [0.0f64; NORMAL_ZIGGURAT_LAYERS];
    let mut f = [0.0f64; NORMAL_ZIGGURAT_LAYERS];
    let integer_scale = 2_147_483_648.0f64;
    let strip_area = 9.912_563_035_262_17e-3f64;
    let mut boundary = NORMAL_ZIGGURAT_R;
    let mut previous_boundary = boundary;
    let tail_width = strip_area / (-0.5 * boundary * boundary).exp();

    k[0] = (boundary / tail_width * integer_scale) as u32;
    k[1] = 0;
    w[0] = tail_width / integer_scale;
    w[NORMAL_ZIGGURAT_LAYERS - 1] = boundary / integer_scale;
    f[0] = 1.0;
    f[NORMAL_ZIGGURAT_LAYERS - 1] = (-0.5 * boundary * boundary).exp();

    for layer in (1..(NORMAL_ZIGGURAT_LAYERS - 1)).rev() {
        boundary =
            (-2.0 * (strip_area / boundary + (-0.5 * boundary * boundary).exp()).ln()).sqrt();
        k[layer + 1] = (boundary / previous_boundary * integer_scale) as u32;
        previous_boundary = boundary;
        f[layer] = (-0.5 * boundary * boundary).exp();
        w[layer] = boundary / integer_scale;
    }
    (k, w, f)
}

#[inline(always)]
fn splitmix64(x: &mut u64) -> u64 {
    *x = x.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *x;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

#[derive(Clone, Copy)]
struct CovarianceSummary {
    samples: usize,
    min_variance: f64,
    mean_variance: f64,
    max_variance: f64,
    min_conditional_sd: Option<f64>,
    mean_conditional_sd: Option<f64>,
    max_conditional_sd: Option<f64>,
    max_abs_correlation: Option<f64>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SwitchSource {
    Greedy,
    Declared(usize),
}

impl SwitchSource {
    fn name(&self) -> &'static str {
        match self {
            SwitchSource::Greedy => "greedy",
            SwitchSource::Declared(_) => "declared",
        }
    }

    fn declaration_index(&self) -> Option<usize> {
        match self {
            SwitchSource::Greedy => None,
            SwitchSource::Declared(index) => Some(*index),
        }
    }
}

#[derive(Clone, Debug)]
struct SwitchPair {
    position_j: usize,
    position_k: usize,
    center_j: f64,
    center_k: f64,
    sd_j: f64,
    sd_k: f64,
    correlation: f64,
    source: SwitchSource,
    warmup_attempts: u64,
    warmup_accepts: u64,
    sampling_attempts: u64,
    sampling_accepts: u64,
}

impl SwitchPair {
    fn new(
        position_j: usize,
        position_k: usize,
        center_j: f64,
        center_k: f64,
        sd_j: f64,
        sd_k: f64,
        correlation: f64,
        source: SwitchSource,
    ) -> Self {
        Self {
            position_j,
            position_k,
            center_j,
            center_k,
            sd_j,
            sd_k,
            correlation,
            source,
            warmup_attempts: 0,
            warmup_accepts: 0,
            sampling_attempts: 0,
            sampling_accepts: 0,
        }
    }

    #[inline]
    fn proposed_values(&self, current_j: f64, current_k: f64) -> Option<(f64, f64)> {
        if !(current_j.is_finite()
            && current_k.is_finite()
            && self.sd_j.is_finite()
            && self.sd_j > 0.0
            && self.sd_k.is_finite()
            && self.sd_k > 0.0)
        {
            return None;
        }
        let z_j = (current_j - self.center_j) / self.sd_j;
        let z_k = (current_k - self.center_k) / self.sd_k;
        let (new_z_j, new_z_k) = if self.correlation < 0.0 {
            (z_k, z_j)
        } else {
            (-z_k, -z_j)
        };
        let proposed_j = self.center_j + self.sd_j * new_z_j;
        let proposed_k = self.center_k + self.sd_k * new_z_k;
        if proposed_j.is_finite() && proposed_k.is_finite() {
            Some((proposed_j, proposed_k))
        } else {
            None
        }
    }

    #[inline]
    fn record(&mut self, accepted: bool, warmup: bool) {
        if warmup {
            self.warmup_attempts += 1;
            self.warmup_accepts += if accepted { 1 } else { 0 };
        } else {
            self.sampling_attempts += 1;
            self.sampling_accepts += if accepted { 1 } else { 0 };
        }
    }

    fn total_attempts(&self) -> u64 {
        self.warmup_attempts + self.sampling_attempts
    }

    fn total_accepts(&self) -> u64 {
        self.warmup_accepts + self.sampling_accepts
    }
}

#[derive(Clone, Copy, Debug)]
struct GeometrySchedule {
    collect_start: usize,
    fit_iter: usize,
}

impl GeometrySchedule {
    fn new(adapt_until: usize) -> Self {
        if adapt_until == 0 {
            return Self {
                collect_start: 0,
                fit_iter: 0,
            };
        }
        // The first 40% of adaptive warmup is scalar-only. Discard the first
        // half of that stage, accumulate geometry over the second half, then
        // fit it exactly once. The remaining 60% tunes scalar RW scales while
        // the frozen switches run.
        let forty_percent = adapt_until.saturating_mul(2).saturating_add(4) / 5;
        let fit_iter = forty_percent.max(20).min(adapt_until);
        let collect_start = (fit_iter / 2).max(1);
        Self {
            collect_start,
            fit_iter,
        }
    }

    #[inline]
    fn should_collect(&self, iter: usize) -> bool {
        self.fit_iter > 0 && iter >= self.collect_start && iter <= self.fit_iter
    }
}

struct DeclaredSwitchTrainer {
    specs: Vec<DeclaredSwitchSpec>,
    unique_positions: Vec<usize>,
    edge_locals: Vec<(usize, usize)>,
    n: usize,
    mean: Vec<f64>,
    m2: Vec<f64>,
    delta: Vec<f64>,
    co_moments: Vec<f64>,
}

impl DeclaredSwitchTrainer {
    fn new(specs: Vec<DeclaredSwitchSpec>) -> Self {
        // A cycle such as beta(1)~u(1), ..., beta(1)~u(m) repeats beta(1)
        // in every edge. Train each distinct coordinate's center and variance
        // once per sweep, then retain only one cross-moment per declared edge.
        // This keeps storage O(V + E) and avoids recalculating the repeated
        // coordinate moments E times.
        let mut unique_positions = Vec::with_capacity(specs.len().saturating_mul(2));
        for spec in &specs {
            unique_positions.push(spec.position_j);
            unique_positions.push(spec.position_k);
        }
        unique_positions.sort_unstable();
        unique_positions.dedup();

        let edge_locals = specs
            .iter()
            .map(|spec| {
                let local_j = unique_positions
                    .binary_search(&spec.position_j)
                    .expect("declared switch position_j must be indexed");
                let local_k = unique_positions
                    .binary_search(&spec.position_k)
                    .expect("declared switch position_k must be indexed");
                (local_j, local_k)
            })
            .collect();
        let coordinate_count = unique_positions.len();
        let edge_count = specs.len();
        Self {
            specs,
            unique_positions,
            edge_locals,
            n: 0,
            mean: vec![0.0; coordinate_count],
            m2: vec![0.0; coordinate_count],
            delta: vec![0.0; coordinate_count],
            co_moments: vec![0.0; edge_count],
        }
    }

    #[inline]
    fn update_values(&mut self, values: &[f64]) {
        if self.specs.is_empty() || values.len() != self.unique_positions.len() {
            return;
        }
        self.n += 1;
        let inv_n = 1.0 / self.n as f64;

        for (local, &value) in values.iter().enumerate() {
            let delta = value - self.mean[local];
            self.delta[local] = delta;
            self.mean[local] += delta * inv_n;
            self.m2[local] += delta * (value - self.mean[local]);
        }
        for (edge, &(local_j, local_k)) in self.edge_locals.iter().enumerate() {
            let value_k = values[local_k];
            self.co_moments[edge] +=
                self.delta[local_j] * (value_k - self.mean[local_k]);
        }
    }

    #[inline]
    fn update(&mut self, theta: &[f64], posterior: &PosteriorLib) {
        if self.specs.is_empty() {
            return;
        }
        let mut values = Vec::with_capacity(self.unique_positions.len());
        for &position in &self.unique_positions {
            let Some(value) = posterior.switch_coordinate_value(position, theta) else {
                return;
            };
            values.push(value);
        }
        self.update_values(&values);
    }

    fn pairs(&self) -> Vec<SwitchPair> {
        if self.n < 2 {
            return Vec::new();
        }
        let denom = (self.n - 1) as f64;
        self.specs
            .iter()
            .copied()
            .zip(self.edge_locals.iter().copied())
            .zip(self.co_moments.iter().copied())
            .enumerate()
            .filter_map(|(index, ((spec, (local_j, local_k)), co_moment))| {
                let variance_j = self.m2[local_j] / denom;
                let variance_k = self.m2[local_k] / denom;
                let center_j = self.mean[local_j];
                let center_k = self.mean[local_k];
                if !(variance_j.is_finite()
                    && variance_j > 0.0
                    && variance_k.is_finite()
                    && variance_k > 0.0
                    && center_j.is_finite()
                    && center_k.is_finite())
                {
                    return None;
                }
                let covariance = co_moment / denom;
                let correlation = covariance / (variance_j * variance_k).sqrt();
                if !correlation.is_finite() {
                    return None;
                }
                Some(SwitchPair::new(
                    spec.position_j,
                    spec.position_k,
                    center_j,
                    center_k,
                    variance_j.sqrt(),
                    variance_k.sqrt(),
                    correlation.clamp(-1.0, 1.0),
                    SwitchSource::Declared(index + 1),
                ))
            })
            .collect()
    }
}

fn same_unordered_pair(left: &SwitchPair, right: &SwitchPair) -> bool {
    (left.position_j == right.position_j && left.position_k == right.position_k)
        || (left.position_j == right.position_k && left.position_k == right.position_j)
}

fn combine_switch_pairs(
    mut declared: Vec<SwitchPair>,
    greedy: Vec<SwitchPair>,
) -> Vec<SwitchPair> {
    // User-declared pairs are kept in source order so a `for` declaration is
    // a deterministic cycle. Greedy pairs are appended, with exact duplicate
    // edges removed. Overlapping declared pairs are intentional and valid:
    // each pair is a separate MH kernel applied sequentially.
    for pair in greedy {
        if !declared.iter().any(|existing| same_unordered_pair(existing, &pair)) {
            declared.push(pair);
        }
    }
    declared
}

struct SwitchController {
    enabled: bool,
    threshold: f64,
    schedule: GeometrySchedule,
    active: bool,
    pairs: Vec<SwitchPair>,
}

impl SwitchController {
    fn new(enabled: bool, threshold: f64, schedule: GeometrySchedule) -> Self {
        Self {
            enabled,
            threshold,
            schedule,
            active: false,
            pairs: Vec::new(),
        }
    }

    fn activate(&mut self, declared: Vec<SwitchPair>, greedy: Vec<SwitchPair>) {
        if !self.enabled || self.active {
            return;
        }
        self.pairs = combine_switch_pairs(declared, greedy);
        self.active = true;
    }

    fn record(&mut self, pair_index: usize, accepted: bool, warmup: bool) {
        self.pairs[pair_index].record(accepted, warmup);
    }

    fn write_diagnostics(&self, path: &str) {
        let file = File::create(path).unwrap_or_else(|error| {
            eprintln!("could not create switch diagnostics {}: {}", path, error);
            process::exit(1);
        });
        let mut writer = BufWriter::with_capacity(1 << 16, file);
        writeln!(
            writer,
            "pair,source,declaration,position_j,position_k,correlation,switch_type,center_j,center_k,sd_j,sd_k,warmup_attempts,warmup_accepts,warmup_acceptance,sampling_attempts,sampling_accepts,sampling_acceptance,total_attempts,total_accepts,total_acceptance,training_start,training_end"
        )
        .unwrap();
        for (index, pair) in self.pairs.iter().enumerate() {
            let warmup_rate = pair.warmup_accepts as f64 / pair.warmup_attempts.max(1) as f64;
            let sampling_rate =
                pair.sampling_accepts as f64 / pair.sampling_attempts.max(1) as f64;
            let total_attempts = pair.total_attempts();
            let total_accepts = pair.total_accepts();
            let total_rate = total_accepts as f64 / total_attempts.max(1) as f64;
            let switch_type = if pair.correlation < 0.0 {
                "centered_swap"
            } else {
                "centered_signed_swap"
            };
            let declaration = pair
                .source
                .declaration_index()
                .map(|value| value.to_string())
                .unwrap_or_default();
            writeln!(
                writer,
                "{},{},{},{},{},{:.17e},{},{:.17e},{:.17e},{:.17e},{:.17e},{},{},{:.17e},{},{},{:.17e},{},{},{:.17e},{},{}",
                index + 1,
                pair.source.name(),
                declaration,
                pair.position_j + 1,
                pair.position_k + 1,
                pair.correlation,
                switch_type,
                pair.center_j,
                pair.center_k,
                pair.sd_j,
                pair.sd_k,
                pair.warmup_attempts,
                pair.warmup_accepts,
                warmup_rate,
                pair.sampling_attempts,
                pair.sampling_accepts,
                sampling_rate,
                total_attempts,
                total_accepts,
                total_rate,
                self.schedule.collect_start,
                self.schedule.fit_iter,
            )
            .unwrap();
        }
        writer.flush().unwrap();
    }
}

fn finite_summary(values: &[f64]) -> (f64, f64, f64) {
    let mut min_value = f64::INFINITY;
    let mut max_value = f64::NEG_INFINITY;
    let mut sum = 0.0;
    let mut count = 0usize;
    for &value in values {
        if value.is_finite() && value >= 0.0 {
            min_value = min_value.min(value);
            max_value = max_value.max(value);
            sum += value;
            count += 1;
        }
    }
    if count == 0 {
        (f64::NAN, f64::NAN, f64::NAN)
    } else {
        (min_value, sum / count as f64, max_value)
    }
}

fn covariance_summary(
    samples: usize,
    variances: &[f64],
    conditional_sds: Option<&[f64]>,
    max_abs_correlation: Option<f64>,
) -> CovarianceSummary {
    let (min_variance, mean_variance, max_variance) = finite_summary(variances);
    let (min_conditional_sd, mean_conditional_sd, max_conditional_sd) =
        if let Some(values) = conditional_sds {
            let (min_value, mean_value, max_value) = finite_summary(values);
            (Some(min_value), Some(mean_value), Some(max_value))
        } else {
            (None, None, None)
        };
    CovarianceSummary {
        samples,
        min_variance,
        mean_variance,
        max_variance,
        min_conditional_sd,
        mean_conditional_sd,
        max_conditional_sd,
        max_abs_correlation,
    }
}

struct WarmupDiagonalMoments {
    n: usize,
    mean: Vec<f64>,
    m2: Vec<f64>,
    last_shapes: Vec<f64>,
}

impl WarmupDiagonalMoments {
    fn new(dim: usize) -> Self {
        Self {
            n: 0,
            mean: vec![0.0; dim],
            m2: vec![0.0; dim],
            last_shapes: vec![1.0; dim],
        }
    }

    #[inline]
    fn update(&mut self, theta: &[f64], positions: &[usize]) {
        self.n += 1;
        let inv_n = 1.0 / self.n as f64;
        for (local, &position) in positions.iter().enumerate() {
            let value = theta[position];
            let delta = value - self.mean[local];
            self.mean[local] += delta * inv_n;
            self.m2[local] += delta * (value - self.mean[local]);
        }
    }

    fn fill_shape_suggestions(&mut self, out: &mut [f64]) -> bool {
        if self.n < 5 || out.len() != self.m2.len() {
            return false;
        }
        let denom = (self.n - 1) as f64;
        let mut mean_variance = 0.0;
        let mut positive = 0usize;
        for &m2 in &self.m2 {
            let variance = m2 / denom;
            if variance.is_finite() && variance > 0.0 {
                mean_variance += variance;
                positive += 1;
            }
        }
        if positive == 0 {
            return false;
        }
        mean_variance /= positive as f64;
        let ridge = (mean_variance * 1e-8).max(1e-12);
        for (shape, &m2) in out.iter_mut().zip(&self.m2) {
            let variance = (m2 / denom).max(0.0);
            *shape = (variance + ridge).sqrt();
        }
        self.last_shapes.clone_from_slice(out);
        true
    }

    fn summary(&self) -> CovarianceSummary {
        if self.n < 2 {
            return covariance_summary(self.n, &[], None, None);
        }
        let denom = (self.n - 1) as f64;
        let variances: Vec<f64> = self.m2.iter().map(|value| *value / denom).collect();
        covariance_summary(self.n, &variances, Some(&self.last_shapes), None)
    }
}

struct WarmupPackedMoments {
    n: usize,
    dim: usize,
    mean: Vec<f64>,
    delta: Vec<f64>,
    m2: Vec<f64>,
    last_shapes: Vec<f64>,
    variances_work: Vec<f64>,
    chol_work: Vec<f64>,
    solve_work: Vec<f64>,
}

impl WarmupPackedMoments {
    fn new(dim: usize) -> Self {
        let dim_plus_one = dim.checked_add(1).unwrap_or_else(|| {
            eprintln!("full covariance dimension is too large");
            process::exit(2);
        });
        let packed_len = dim
            .checked_mul(dim_plus_one)
            .and_then(|value| value.checked_div(2))
            .unwrap_or_else(|| {
                eprintln!("full covariance dimension is too large");
                process::exit(2);
            });
        Self {
            n: 0,
            dim,
            mean: vec![0.0; dim],
            delta: vec![0.0; dim],
            m2: vec![0.0; packed_len],
            last_shapes: vec![1.0; dim],
            variances_work: vec![0.0; dim],
            // Cholesky workspaces are allocated lazily. Correlation switches
            // need packed moments but do not need a dense dim-by-dim matrix.
            chol_work: Vec::new(),
            solve_work: Vec::new(),
        }
    }

    #[inline]
    fn update(&mut self, theta: &[f64], positions: &[usize]) {
        self.n += 1;
        let inv_n = 1.0 / self.n as f64;
        for (local, &position) in positions.iter().enumerate() {
            let value = theta[position];
            let delta = value - self.mean[local];
            self.delta[local] = delta;
            self.mean[local] += delta * inv_n;
        }
        for i in 0..self.dim {
            let row = i * (i + 1) / 2;
            let di = self.delta[i];
            for j in 0..=i {
                let value_j = theta[positions[j]];
                self.m2[row + j] += di * (value_j - self.mean[j]);
            }
        }
    }

    fn variances(&self) -> Vec<f64> {
        if self.n < 2 {
            return Vec::new();
        }
        let denom = (self.n - 1) as f64;
        let mut variances = vec![0.0; self.dim];
        for i in 0..self.dim {
            variances[i] = self.m2[i * (i + 1) / 2 + i] / denom;
        }
        variances
    }

    fn fill_marginal_shape_suggestions(&mut self, out: &mut [f64]) -> bool {
        if self.n < 5 || self.dim == 0 || out.len() != self.dim {
            return false;
        }
        let denom = (self.n - 1) as f64;
        let mut mean_variance = 0.0;
        let mut positive = 0usize;
        for i in 0..self.dim {
            let variance = self.m2[i * (i + 1) / 2 + i] / denom;
            self.variances_work[i] = variance;
            if variance.is_finite() && variance > 0.0 {
                mean_variance += variance;
                positive += 1;
            }
        }
        if positive == 0 {
            return false;
        }
        mean_variance /= positive as f64;
        let ridge = (mean_variance * 1e-8).max(1e-12);
        for (value, &variance) in out.iter_mut().zip(&self.variances_work) {
            *value = (variance.max(0.0) + ridge).sqrt();
        }
        self.last_shapes.clone_from_slice(out);
        true
    }

    fn greedy_switch_pairs(&self, positions: &[usize], threshold: f64) -> Vec<SwitchPair> {
        if self.n < 2 || self.dim < 2 || positions.len() != self.dim {
            return Vec::new();
        }
        let denom = (self.n - 1) as f64;
        let variances = self.variances();
        let valid: Vec<bool> = (0..self.dim)
            .map(|local| {
                let variance = variances[local];
                variance.is_finite() && variance > 0.0 && self.mean[local].is_finite()
            })
            .collect();

        // Sorting every eligible edge once is equivalent to repeatedly taking
        // the strongest edge remaining after its two endpoints are removed,
        // but reduces pair construction from a repeated cubic scan to
        // O(p^2 log p).  The local-index tie breakers preserve deterministic
        // behavior when two correlations have exactly the same magnitude.
        let edge_capacity = self
            .dim
            .checked_mul(self.dim.saturating_sub(1))
            .and_then(|value| value.checked_div(2))
            .unwrap_or(0);
        let mut edges: Vec<(f64, usize, usize, f64)> = Vec::with_capacity(edge_capacity);
        for i in 0..self.dim {
            if !valid[i] {
                continue;
            }
            let variance_i = variances[i];
            for j in (i + 1)..self.dim {
                if !valid[j] {
                    continue;
                }
                let variance_j = variances[j];
                let covariance = self.m2[j * (j + 1) / 2 + i] / denom;
                let correlation = covariance / (variance_i * variance_j).sqrt();
                let magnitude = correlation.abs();
                if correlation.is_finite() && magnitude > threshold {
                    edges.push((magnitude, i, j, correlation));
                }
            }
        }
        edges.sort_by(|left, right| {
            right
                .0
                .total_cmp(&left.0)
                .then_with(|| left.1.cmp(&right.1))
                .then_with(|| left.2.cmp(&right.2))
        });

        let mut used = vec![false; self.dim];
        let mut pairs = Vec::with_capacity(self.dim / 2);
        for (_, local_j, local_k, correlation) in edges {
            if used[local_j] || used[local_k] {
                continue;
            }
            used[local_j] = true;
            used[local_k] = true;
            pairs.push(SwitchPair::new(
                positions[local_j],
                positions[local_k],
                self.mean[local_j],
                self.mean[local_k],
                variances[local_j].sqrt(),
                variances[local_k].sqrt(),
                correlation,
                SwitchSource::Greedy,
            ));
        }
        pairs
    }

    fn fill_shape_suggestions(&mut self, out: &mut [f64]) -> bool {
        if self.n < 5 || self.dim == 0 || out.len() != self.dim {
            return false;
        }
        let denom = (self.n - 1) as f64;
        let mut mean_variance = 0.0;
        let mut positive = 0usize;
        for i in 0..self.dim {
            let variance = self.m2[i * (i + 1) / 2 + i] / denom;
            self.variances_work[i] = variance;
            if variance.is_finite() && variance > 0.0 {
                mean_variance += variance;
                positive += 1;
            }
        }
        if positive == 0 {
            return false;
        }
        mean_variance /= positive as f64;

        let dense_len = self.dim.checked_mul(self.dim).unwrap_or_else(|| {
            eprintln!("full covariance workspace is too large");
            process::exit(2);
        });
        if self.chol_work.len() != dense_len {
            self.chol_work.resize(dense_len, 0.0);
        }
        if self.solve_work.len() != self.dim {
            self.solve_work.resize(self.dim, 0.0);
        }

        // Early empirical covariance estimates are rank deficient and noisy.
        // Shrink off-diagonal entries toward zero, then add a small ridge. The
        // shrinkage fades automatically as the adaptation history grows.
        let shrink = (((self.dim + 1) as f64) / denom).clamp(0.0, 0.95);
        let offdiag_weight = 1.0 - shrink;
        let base_ridge = (mean_variance * 1e-8).max(1e-12);
        let mut ridge = base_ridge;
        let mut factored = false;

        for _ in 0..8 {
            for i in 0..self.dim {
                let row = i * (i + 1) / 2;
                for j in 0..=i {
                    let raw = self.m2[row + j] / denom;
                    let value = if i == j {
                        raw.max(0.0) + ridge
                    } else {
                        raw * offdiag_weight
                    };
                    self.chol_work[i * self.dim + j] = value;
                    self.chol_work[j * self.dim + i] = value;
                }
            }
            if cholesky_lower_in_place(&mut self.chol_work, self.dim) {
                factored = true;
                break;
            }
            ridge *= 10.0;
        }
        if !factored {
            return false;
        }

        // For covariance Sigma = L L', the conditional variance of coordinate
        // j given all other coordinates is 1 / (Sigma^{-1})[j,j]. The diagonal
        // of Sigma^{-1} is obtained from columns of L^{-1}. Proposals remain
        // scalar; this only supplies a scale for each one-dimensional move.
        let floor_sd = mean_variance.sqrt() * 1e-8 + 1e-12;
        for column in 0..self.dim {
            self.solve_work.fill(0.0);
            for row in 0..self.dim {
                let mut rhs = if row == column { 1.0 } else { 0.0 };
                for k in 0..row {
                    rhs -= self.chol_work[row * self.dim + k] * self.solve_work[k];
                }
                self.solve_work[row] = rhs / self.chol_work[row * self.dim + row];
            }
            let precision_diagonal: f64 = self.solve_work.iter().map(|value| value * value).sum();
            let marginal_sd = (self.variances_work[column].max(0.0) + ridge).sqrt();
            let conditional_sd = if precision_diagonal.is_finite() && precision_diagonal > 0.0 {
                (1.0 / precision_diagonal).sqrt()
            } else {
                marginal_sd
            };
            out[column] = conditional_sd.min(marginal_sd).max(floor_sd);
        }
        self.last_shapes.clone_from_slice(out);
        true
    }

    fn summary(&self) -> CovarianceSummary {
        let variances = self.variances();
        if self.n < 2 {
            return covariance_summary(self.n, &variances, None, Some(f64::NAN));
        }
        let denom = (self.n - 1) as f64;
        let mut max_abs_correlation = 0.0f64;
        for i in 1..self.dim {
            let row = i * (i + 1) / 2;
            let variance_i = variances[i];
            if !(variance_i > 0.0) {
                continue;
            }
            for j in 0..i {
                let variance_j = variances[j];
                if !(variance_j > 0.0) {
                    continue;
                }
                let covariance = self.m2[row + j] / denom;
                let correlation = (covariance / (variance_i * variance_j).sqrt()).abs();
                if correlation.is_finite() {
                    max_abs_correlation = max_abs_correlation.max(correlation);
                }
            }
        }
        covariance_summary(
            self.n,
            &variances,
            Some(&self.last_shapes),
            Some(max_abs_correlation),
        )
    }
}

fn cholesky_lower_in_place(matrix: &mut [f64], dim: usize) -> bool {
    for i in 0..dim {
        for j in 0..=i {
            let mut sum = matrix[i * dim + j];
            for k in 0..j {
                sum -= matrix[i * dim + k] * matrix[j * dim + k];
            }
            if i == j {
                if !sum.is_finite() || sum <= 0.0 {
                    return false;
                }
                matrix[i * dim + j] = sum.sqrt();
            } else {
                let diagonal = matrix[j * dim + j];
                if !diagonal.is_finite() || diagonal <= 0.0 {
                    return false;
                }
                matrix[i * dim + j] = sum / diagonal;
            }
        }
        for j in (i + 1)..dim {
            matrix[i * dim + j] = 0.0;
        }
    }
    true
}

enum CovarianceState {
    Full(WarmupPackedMoments),
    Diagonal(WarmupDiagonalMoments),
    Off,
}

struct CovarianceAdapter {
    positions: Vec<usize>,
    shape_work: Vec<f64>,
    state: CovarianceState,
    shape_mode: ShapeAdaptationMode,
    greedy_switches_enabled: bool,
}

impl CovarianceAdapter {
    fn new(
        mode: CovarianceMode,
        positions: Vec<usize>,
        max_full_dim: usize,
        require_full_moments: bool,
        switch_max_dim: usize,
    ) -> Self {
        let dim = positions.len();
        let shape_mode = match mode {
            CovarianceMode::Full => ShapeAdaptationMode::ConditionalFull,
            CovarianceMode::Diagonal => ShapeAdaptationMode::MarginalDiagonal,
            CovarianceMode::Off => ShapeAdaptationMode::Off,
            CovarianceMode::Auto => {
                if dim <= max_full_dim {
                    ShapeAdaptationMode::ConditionalFull
                } else {
                    ShapeAdaptationMode::MarginalDiagonal
                }
            }
        };
        // Dense automatic pair discovery is optional in large models. Explicit
        // switch cycles are trained separately with O(V + E) storage for the V
        // distinct coordinates and E declared edges, and remain available above
        // this limit.
        let greedy_switches_enabled = require_full_moments && dim <= switch_max_dim;
        let need_full_state =
            greedy_switches_enabled || shape_mode == ShapeAdaptationMode::ConditionalFull;
        let state = if dim == 0 {
            CovarianceState::Off
        } else if need_full_state {
            CovarianceState::Full(WarmupPackedMoments::new(dim))
        } else if shape_mode == ShapeAdaptationMode::MarginalDiagonal {
            CovarianceState::Diagonal(WarmupDiagonalMoments::new(dim))
        } else {
            CovarianceState::Off
        };
        Self {
            shape_work: vec![1.0; dim],
            positions,
            state,
            shape_mode,
            greedy_switches_enabled,
        }
    }

    fn name(&self) -> &'static str {
        match (self.shape_mode, self.greedy_switches_enabled) {
            (ShapeAdaptationMode::ConditionalFull, true) => {
                "one-time full covariance -> conditional scalar scales + greedy switches"
            }
            (ShapeAdaptationMode::ConditionalFull, false) => {
                "one-time full covariance -> conditional scalar scales"
            }
            (ShapeAdaptationMode::MarginalDiagonal, true) => {
                "one-time diagonal scalar scales; full moments for greedy switches"
            }
            (ShapeAdaptationMode::MarginalDiagonal, false) => {
                "one-time diagonal covariance -> marginal scalar scales"
            }
            (ShapeAdaptationMode::Off, true) => {
                "scale adaptation off; one-time full moments for greedy switches"
            }
            (ShapeAdaptationMode::Off, false) => "off",
        }
    }

    #[inline]
    fn update(&mut self, theta: &[f64]) {
        match &mut self.state {
            CovarianceState::Full(model) => model.update(theta, &self.positions),
            CovarianceState::Diagonal(model) => model.update(theta, &self.positions),
            CovarianceState::Off => {}
        }
    }

    fn fit_shapes_once(&mut self, applied_shapes: &mut [f64]) -> bool {
        let has_suggestions = match self.shape_mode {
            ShapeAdaptationMode::ConditionalFull => match &mut self.state {
                CovarianceState::Full(model) => {
                    model.fill_shape_suggestions(&mut self.shape_work)
                }
                _ => false,
            },
            ShapeAdaptationMode::MarginalDiagonal => match &mut self.state {
                CovarianceState::Full(model) => {
                    model.fill_marginal_shape_suggestions(&mut self.shape_work)
                }
                CovarianceState::Diagonal(model) => {
                    model.fill_shape_suggestions(&mut self.shape_work)
                }
                CovarianceState::Off => false,
            },
            ShapeAdaptationMode::Off => false,
        };
        if !has_suggestions {
            return false;
        }

        // Geometry is deliberately fitted only once. The remaining adaptive
        // warmup sweeps retune scalar Robbins-Monro factors toward 0.44 under
        // these frozen covariance shapes.
        let mut changed = false;
        for (local, &position) in self.positions.iter().enumerate() {
            let suggested = self.shape_work[local];
            if suggested.is_finite() && suggested > 0.0 {
                applied_shapes[position] = suggested;
                changed = true;
            }
        }
        changed
    }

    fn greedy_enabled(&self) -> bool {
        self.greedy_switches_enabled
    }

    fn switch_pairs(&self, threshold: f64) -> Vec<SwitchPair> {
        if !self.greedy_switches_enabled {
            return Vec::new();
        }
        match &self.state {
            CovarianceState::Full(model) => {
                model.greedy_switch_pairs(&self.positions, threshold)
            }
            CovarianceState::Diagonal(_) | CovarianceState::Off => Vec::new(),
        }
    }

    fn summary(&self) -> CovarianceSummary {
        match &self.state {
            CovarianceState::Full(model) => model.summary(),
            CovarianceState::Diagonal(model) => model.summary(),
            CovarianceState::Off => covariance_summary(0, &[], None, None),
        }
    }

    fn coordinate_moments(&self, position: usize) -> (Option<f64>, Option<f64>) {
        let Some(local) = self.positions.iter().position(|&value| value == position) else {
            return (None, None);
        };
        match &self.state {
            CovarianceState::Full(model) => {
                let mean = model
                    .mean
                    .get(local)
                    .copied()
                    .filter(|value| value.is_finite());
                let variance = if model.n >= 2 {
                    let value = model.m2[local * (local + 1) / 2 + local]
                        / (model.n - 1) as f64;
                    value.is_finite().then_some(value.max(0.0))
                } else {
                    None
                };
                (mean, variance)
            }
            CovarianceState::Diagonal(model) => {
                let mean = model
                    .mean
                    .get(local)
                    .copied()
                    .filter(|value| value.is_finite());
                let variance = if model.n >= 2 {
                    let value = model.m2[local] / (model.n - 1) as f64;
                    value.is_finite().then_some(value.max(0.0))
                } else {
                    None
                };
                (mean, variance)
            }
            CovarianceState::Off => (None, None),
        }
    }

    fn write_covariance_diagnostics(
        &self,
        path: &str,
        training_start: usize,
        training_end: usize,
    ) {
        let file = File::create(path).unwrap_or_else(|error| {
            eprintln!(
                "could not create covariance diagnostics {}: {}",
                path, error
            );
            process::exit(1);
        });
        let mut writer = BufWriter::with_capacity(1 << 20, file);
        writeln!(
            writer,
            "mode,position_i,position_j,covariance,correlation,samples,training_start,training_end"
        )
        .unwrap();

        match &self.state {
            CovarianceState::Full(model) => {
                let denom = if model.n >= 2 {
                    Some((model.n - 1) as f64)
                } else {
                    None
                };
                let variances = model.variances();
                for i in 0..model.dim {
                    let row = i * (i + 1) / 2;
                    for j in 0..=i {
                        let covariance = denom.and_then(|value| {
                            let out = model.m2[row + j] / value;
                            out.is_finite().then_some(out)
                        });
                        let correlation = covariance.and_then(|value| {
                            let vi = variances.get(i).copied().unwrap_or(f64::NAN);
                            let vj = variances.get(j).copied().unwrap_or(f64::NAN);
                            if vi > 0.0 && vj > 0.0 {
                                let out = value / (vi * vj).sqrt();
                                out.is_finite().then_some(out.clamp(-1.0, 1.0))
                            } else {
                                None
                            }
                        });
                        writeln!(
                            writer,
                            "full,{},{},{},{},{},{},{}",
                            self.positions[i] + 1,
                            self.positions[j] + 1,
                            format_optional_f64(covariance),
                            format_optional_f64(correlation),
                            model.n,
                            training_start,
                            training_end
                        )
                        .unwrap();
                    }
                }
            }
            CovarianceState::Diagonal(model) => {
                for (local, &position) in self.positions.iter().enumerate() {
                    let variance = if model.n >= 2 {
                        let value = model.m2[local] / (model.n - 1) as f64;
                        value.is_finite().then_some(value.max(0.0))
                    } else {
                        None
                    };
                    let correlation = variance.map(|_| 1.0);
                    writeln!(
                        writer,
                        "diagonal,{0},{0},{1},{2},{3},{4},{5}",
                        position + 1,
                        format_optional_f64(variance),
                        format_optional_f64(correlation),
                        model.n,
                        training_start,
                        training_end
                    )
                    .unwrap();
                }
            }
            CovarianceState::Off => {}
        }
        writer.flush().unwrap();
    }
}

fn format_optional_f64(value: Option<f64>) -> String {
    match value {
        Some(number) if number.is_finite() => format!("{number:.17e}"),
        _ => String::new(),
    }
}

#[allow(clippy::too_many_arguments)]
fn write_adaptation_diagnostics(
    path: &str,
    value_kinds: &[ValueKind],
    proposal_counts: &[u64],
    accepted_counts: &[u64],
    warmup_proposal_counts: &[u64],
    warmup_accepted_counts: &[u64],
    proposal_scales: &[f64],
    tuning_factors: &[f64],
    covariance_shapes: &[f64],
    covariance: &CovarianceAdapter,
) {
    let file = File::create(path).unwrap_or_else(|error| {
        eprintln!(
            "could not create scalar adaptation diagnostics {}: {}",
            path, error
        );
        process::exit(1);
    });
    let mut writer = BufWriter::with_capacity(1 << 20, file);
    writeln!(
        writer,
        "position,value_kind,proposals,accepted,acceptance_rate,warmup_proposals,warmup_accepted,warmup_acceptance_rate,proposal_sd,tuning_factor,covariance_shape,warmup_mean,warmup_variance"
    )
    .unwrap();

    for position in 0..value_kinds.len() {
        let proposals = proposal_counts[position];
        let accepted = accepted_counts[position];
        let warmup_proposals = warmup_proposal_counts[position];
        let warmup_accepted = warmup_accepted_counts[position];
        let acceptance_rate = (proposals > 0).then_some(accepted as f64 / proposals as f64);
        let warmup_acceptance_rate =
            (warmup_proposals > 0).then_some(warmup_accepted as f64 / warmup_proposals as f64);
        let is_continuous = value_kinds[position] == ValueKind::Continuous;
        let (warmup_mean, warmup_variance) = covariance.coordinate_moments(position);
        writeln!(
            writer,
            "{},{},{},{},{},{},{},{},{},{},{},{},{}",
            position + 1,
            value_kinds[position].as_str(),
            proposals,
            accepted,
            format_optional_f64(acceptance_rate),
            warmup_proposals,
            warmup_accepted,
            format_optional_f64(warmup_acceptance_rate),
            format_optional_f64(is_continuous.then_some(proposal_scales[position])),
            format_optional_f64(is_continuous.then_some(tuning_factors[position])),
            format_optional_f64(is_continuous.then_some(covariance_shapes[position])),
            format_optional_f64(warmup_mean),
            format_optional_f64(warmup_variance)
        )
        .unwrap();
    }
    writer.flush().unwrap();
}

#[inline(always)]
fn adaptation_multipliers(sweep: usize, target_accept: f64) -> (f64, f64) {
    // Each continuous coordinate is attempted exactly once per sweep, so this
    // is also that coordinate's own Robbins-Monro update count. The old code
    // used a counter incremented for every coordinate and therefore adapted
    // later coordinates much too slowly.
    let gain = ((sweep as f64) + 10.0).powf(-0.6);
    (
        (gain * (1.0 - target_accept)).exp(),
        (-gain * target_accept).exp(),
    )
}

#[inline(always)]
fn compose_proposal_scale(
    initial_step: f64,
    tuning_factor: f64,
    covariance_shape: f64,
    min_scale: f64,
    max_scale: f64,
) -> f64 {
    (initial_step * tuning_factor * covariance_shape).clamp(min_scale, max_scale)
}

#[inline(always)]
fn adapt_tuning_factor(
    tuning_factor: &mut f64,
    accepted: bool,
    up: f64,
    down: f64,
    min_tuning: f64,
    max_tuning: f64,
) {
    *tuning_factor =
        (*tuning_factor * if accepted { up } else { down }).clamp(min_tuning, max_tuning);
}

#[inline]
fn rebuild_proposal_scales(
    positions: &[usize],
    initial_step: f64,
    tuning_factors: &[f64],
    covariance_shapes: &[f64],
    proposal_scales: &mut [f64],
    min_scale: f64,
    max_scale: f64,
) {
    for &position in positions {
        proposal_scales[position] = compose_proposal_scale(
            initial_step,
            tuning_factors[position],
            covariance_shapes[position],
            min_scale,
            max_scale,
        );
    }
}

enum ChainWriter {
    Binary(BinaryChainWriter),
    Csv(CsvChainWriter),
    None,
}

impl ChainWriter {
    fn new(cfg: &Config, dim: usize, total_iters: usize, expected_saved: usize) -> Self {
        let Some(path) = cfg.out.as_ref() else {
            return ChainWriter::None;
        };
        match cfg.out_format {
            OutputFormat::Binary => ChainWriter::Binary(BinaryChainWriter::new(
                path,
                dim,
                total_iters,
                expected_saved,
            )),
            OutputFormat::Csv => ChainWriter::Csv(CsvChainWriter::new(path, dim)),
        }
    }

    #[inline(always)]
    fn write_record(&mut self, iter: u64, logp: f64, accepted: bool, theta: &[f64]) {
        match self {
            ChainWriter::Binary(w) => w.write_record(iter, logp, accepted, theta),
            ChainWriter::Csv(w) => w.write_record(iter, logp, accepted, theta),
            ChainWriter::None => {}
        }
    }

    #[inline(always)]
    fn write_selected_record(
        &mut self,
        iter: u64,
        logp: f64,
        accepted: bool,
        theta: &[f64],
        ranges: &[SaveRange],
    ) {
        match self {
            ChainWriter::Binary(w) => w.write_selected_record(iter, logp, accepted, theta, ranges),
            ChainWriter::Csv(w) => w.write_selected_record(iter, logp, accepted, theta, ranges),
            ChainWriter::None => {}
        }
    }

    fn flush(&mut self) {
        match self {
            ChainWriter::Binary(w) => w.flush(),
            ChainWriter::Csv(w) => w.flush(),
            ChainWriter::None => {}
        }
    }

    fn description(&self, cfg: &Config) -> String {
        match self {
            ChainWriter::Binary(_) => {
                format!("{} (binary)", cfg.out.as_deref().unwrap_or("chain.bin"))
            }
            ChainWriter::Csv(_) => format!("{} (csv)", cfg.out.as_deref().unwrap_or("chain.csv")),
            ChainWriter::None => "disabled".to_string(),
        }
    }
}

struct BinaryChainWriter {
    writer: BufWriter<File>,
    buf: Vec<u8>,
}

impl BinaryChainWriter {
    fn new(path: &str, dim: usize, total_iters: usize, expected_saved: usize) -> Self {
        let file = File::create(path).unwrap_or_else(|e| {
            eprintln!("could not create {}: {}", path, e);
            process::exit(1);
        });
        let mut writer = BufWriter::with_capacity(1 << 22, file);
        let mut header = Vec::with_capacity(80);
        header.extend_from_slice(b"hobbs_BIN_V1\0\0\0\0");
        push_u64(&mut header, 1);
        push_u64(&mut header, dim as u64);
        push_u64(&mut header, total_iters as u64);
        push_u64(&mut header, expected_saved as u64);
        push_u64(&mut header, record_size(dim) as u64);
        writer.write_all(&header).unwrap();
        Self {
            writer,
            buf: Vec::with_capacity(1 << 22),
        }
    }

    #[inline(always)]
    fn begin_record(&mut self, iter: u64, logp: f64, accepted: bool) {
        push_u64(&mut self.buf, iter);
        self.buf.push(if accepted { 1 } else { 0 });
        self.buf.extend_from_slice(&[0u8; 7]);
        push_f64(&mut self.buf, logp);
    }

    #[inline(always)]
    fn flush_buffer_if_needed(&mut self) {
        if self.buf.len() >= (1 << 21) {
            self.writer.write_all(&self.buf).unwrap();
            self.buf.clear();
        }
    }

    #[inline(always)]
    fn write_record(&mut self, iter: u64, logp: f64, accepted: bool, theta: &[f64]) {
        self.begin_record(iter, logp, accepted);
        for &v in theta {
            push_f64(&mut self.buf, v);
        }
        self.flush_buffer_if_needed();
    }

    #[inline(always)]
    fn write_selected_record(
        &mut self,
        iter: u64,
        logp: f64,
        accepted: bool,
        theta: &[f64],
        ranges: &[SaveRange],
    ) {
        self.begin_record(iter, logp, accepted);
        for range in ranges {
            for &v in &theta[range.offset..(range.offset + range.len)] {
                push_f64(&mut self.buf, v);
            }
        }
        self.flush_buffer_if_needed();
    }

    fn write_mean_record(&mut self, saved: u64, mean_logp: f64, sums: &[f64], denom: f64) {
        self.begin_record(saved, mean_logp, false);
        for &sum in sums {
            push_f64(&mut self.buf, sum / denom);
        }
        self.flush_buffer_if_needed();
    }

    fn flush(&mut self) {
        if !self.buf.is_empty() {
            self.writer.write_all(&self.buf).unwrap();
            self.buf.clear();
        }
        self.writer.flush().unwrap();
    }
}

struct CsvChainWriter {
    writer: BufWriter<File>,
}

impl CsvChainWriter {
    fn new(path: &str, dim: usize) -> Self {
        let file = File::create(path).unwrap_or_else(|e| {
            eprintln!("could not create {}: {}", path, e);
            process::exit(1);
        });
        let mut writer = BufWriter::with_capacity(1 << 20, file);
        write!(writer, "iter,logp,accepted").unwrap();
        for j in 0..dim {
            write!(writer, ",theta{}", j).unwrap();
        }
        writeln!(writer).unwrap();
        Self { writer }
    }

    #[inline]
    fn write_record(&mut self, iter: u64, logp: f64, accepted: bool, theta: &[f64]) {
        write!(
            self.writer,
            "{},{:.17e},{}",
            iter,
            logp,
            if accepted { 1 } else { 0 }
        )
        .unwrap();
        for v in theta {
            write!(self.writer, ",{:.17e}", v).unwrap();
        }
        writeln!(self.writer).unwrap();
    }

    #[inline]
    fn write_selected_record(
        &mut self,
        iter: u64,
        logp: f64,
        accepted: bool,
        theta: &[f64],
        ranges: &[SaveRange],
    ) {
        write!(
            self.writer,
            "{},{:.17e},{}",
            iter,
            logp,
            if accepted { 1 } else { 0 }
        )
        .unwrap();
        for range in ranges {
            for value in &theta[range.offset..(range.offset + range.len)] {
                write!(self.writer, ",{:.17e}", value).unwrap();
            }
        }
        writeln!(self.writer).unwrap();
    }

    fn flush(&mut self) {
        self.writer.flush().unwrap();
    }
}

#[inline]
fn push_u64(buf: &mut Vec<u8>, x: u64) {
    buf.extend_from_slice(&x.to_le_bytes());
}

#[inline]
fn push_f64(buf: &mut Vec<u8>, x: f64) {
    buf.extend_from_slice(&x.to_le_bytes());
}

#[inline]
fn record_size(dim: usize) -> usize {
    8 + 8 + 8 + dim * 8 // iter + accepted/padding + logp + theta
}

fn print_progress(iter: usize, total: usize, last_percent: &mut usize) {
    if total == 0 {
        return;
    }
    let percent = ((iter.saturating_mul(100)) / total).min(100);
    if percent == *last_percent {
        return;
    }
    *last_percent = percent;
    let width = 30usize;
    let filled = (percent * width) / 100;
    let empty = width - filled;
    eprint!(
        "\r|{}{}|{}%",
        "*".repeat(filled),
        " ".repeat(empty),
        percent
    );
    let _ = std::io::stderr().flush();
}

struct MeanAccumulator {
    n: usize,
    logp_sum: f64,
    theta_sum: Vec<f64>,
}

impl MeanAccumulator {
    fn new(dim: usize) -> Self {
        Self {
            n: 0,
            logp_sum: 0.0,
            theta_sum: vec![0.0; dim],
        }
    }

    #[inline(always)]
    fn update(&mut self, logp: f64, theta: &[f64]) {
        self.n += 1;
        self.logp_sum += logp;
        for (s, &v) in self.theta_sum.iter_mut().zip(theta.iter()) {
            *s += v;
        }
    }

    fn write_csv(&self, path: &str) {
        let file = File::create(path).unwrap_or_else(|e| {
            eprintln!("could not create {}: {}", path, e);
            process::exit(1);
        });
        let mut writer = BufWriter::with_capacity(1 << 20, file);
        write!(writer, "saved,logp").unwrap();
        for j in 0..self.theta_sum.len() {
            write!(writer, ",theta{}", j + 1).unwrap();
        }
        writeln!(writer).unwrap();
        let denom = self.n.max(1) as f64;
        write!(writer, "{},{}", self.n, self.logp_sum / denom).unwrap();
        for &s in &self.theta_sum {
            write!(writer, ",{:.17e}", s / denom).unwrap();
        }
        writeln!(writer).unwrap();
        writer.flush().unwrap();
    }
}

struct SelectedMeanAccumulator {
    n: usize,
    logp_sum: f64,
    theta_sum: Vec<f64>,
    ranges: Vec<SaveRange>,
}

impl SelectedMeanAccumulator {
    fn new(ranges: &[SaveRange]) -> Self {
        let dim = ranges.iter().map(|range| range.len).sum();
        Self {
            n: 0,
            logp_sum: 0.0,
            theta_sum: vec![0.0; dim],
            ranges: ranges.to_vec(),
        }
    }

    #[inline(always)]
    fn update(&mut self, logp: f64, theta: &[f64]) {
        self.n += 1;
        self.logp_sum += logp;
        let mut output_offset = 0usize;
        for range in &self.ranges {
            let source = &theta[range.offset..(range.offset + range.len)];
            let destination = &mut self.theta_sum[output_offset..(output_offset + range.len)];
            for (sum, &value) in destination.iter_mut().zip(source.iter()) {
                *sum += value;
            }
            output_offset += range.len;
        }
    }

    fn write_binary(&self, path: &str, total_iters: usize) {
        let denom = self.n.max(1) as f64;
        let mut writer = BinaryChainWriter::new(path, self.theta_sum.len(), total_iters, 1);
        writer.write_mean_record(self.n as u64, self.logp_sum / denom, &self.theta_sum, denom);
        writer.flush();
    }
}

enum RetainedOutput {
    None,
    Chain(ChainWriter),
    GlobalMean {
        accumulator: MeanAccumulator,
        path: String,
    },
    Split {
        chain_writer: Option<ChainWriter>,
        chain_ranges: Vec<SaveRange>,
        mean_accumulator: SelectedMeanAccumulator,
        mean_path: String,
    },
}

impl RetainedOutput {
    fn new(cfg: &Config, dim: usize, total_iters: usize, expected_saved: usize) -> Self {
        let Some(path) = cfg.out.as_ref() else {
            return RetainedOutput::None;
        };
        if cfg.save_mode == SaveMode::Mean {
            return RetainedOutput::GlobalMean {
                accumulator: MeanAccumulator::new(dim),
                path: path.clone(),
            };
        }
        if cfg.mean_ranges.is_empty() {
            return RetainedOutput::Chain(ChainWriter::new(cfg, dim, total_iters, expected_saved));
        }

        let chain_ranges = complement_save_ranges(dim, &cfg.mean_ranges);
        let chain_dim: usize = chain_ranges.iter().map(|range| range.len).sum();
        let chain_writer = if chain_dim == 0 {
            None
        } else {
            Some(ChainWriter::new(
                cfg,
                chain_dim,
                total_iters,
                expected_saved,
            ))
        };
        RetainedOutput::Split {
            chain_writer,
            chain_ranges,
            mean_accumulator: SelectedMeanAccumulator::new(&cfg.mean_ranges),
            mean_path: cfg.mean_out.clone().unwrap_or_else(|| {
                eprintln!("internal error: declaration-level mean output path is missing");
                process::exit(2);
            }),
        }
    }

    #[inline(always)]
    fn retain(&mut self, iter: u64, logp: f64, accepted: bool, theta: &[f64]) {
        match self {
            RetainedOutput::None => {}
            RetainedOutput::Chain(writer) => writer.write_record(iter, logp, accepted, theta),
            RetainedOutput::GlobalMean { accumulator, .. } => accumulator.update(logp, theta),
            RetainedOutput::Split {
                chain_writer,
                chain_ranges,
                mean_accumulator,
                ..
            } => {
                mean_accumulator.update(logp, theta);
                if let Some(writer) = chain_writer {
                    writer.write_selected_record(iter, logp, accepted, theta, chain_ranges);
                }
            }
        }
    }

    fn finish(&mut self, total_iters: usize) {
        match self {
            RetainedOutput::None => {}
            RetainedOutput::Chain(writer) => writer.flush(),
            RetainedOutput::GlobalMean { accumulator, path } => accumulator.write_csv(path),
            RetainedOutput::Split {
                chain_writer,
                mean_accumulator,
                mean_path,
                ..
            } => {
                if let Some(writer) = chain_writer {
                    writer.flush();
                }
                mean_accumulator.write_binary(mean_path, total_iters);
            }
        }
    }

    fn description(&self, cfg: &Config) -> String {
        match self {
            RetainedOutput::None => "disabled".to_string(),
            RetainedOutput::Chain(writer) => writer.description(cfg),
            RetainedOutput::GlobalMean { path, .. } => {
                format!("{} (posterior mean csv)", path)
            }
            RetainedOutput::Split {
                chain_writer,
                mean_path,
                ..
            } => {
                if let Some(writer) = chain_writer {
                    format!(
                        "{}; {} (one-row posterior mean binary)",
                        writer.description(cfg),
                        mean_path
                    )
                } else {
                    format!("{} (one-row posterior mean binary)", mean_path)
                }
            }
        }
    }
}

#[inline(always)]
fn metropolis_accept(log_alpha: f64, uniform: f64) -> bool {
    if log_alpha.is_nan() || log_alpha == f64::NEG_INFINITY {
        false
    } else if log_alpha >= 0.0 {
        true
    } else {
        uniform < log_alpha.exp()
    }
}

fn discrete_state_count(lower: i64, upper: i64, block_name: &str) -> usize {
    let span = upper
        .checked_sub(lower)
        .and_then(|value| value.checked_add(1))
        .unwrap_or_else(|| {
            eprintln!("discrete range for block {} is too large", block_name);
            process::exit(2);
        });
    usize::try_from(span).unwrap_or_else(|_| {
        eprintln!(
            "discrete range for block {} does not fit in memory",
            block_name
        );
        process::exit(2);
    })
}

#[derive(Clone, Copy)]
struct CoordinateBlockRef {
    block_index: usize,
    scalar_index: c_int,
}

#[inline]
fn block_needs_snapshot_for_rejection(block: &RuntimeBlock) -> bool {
    block.cache_update.is_some()
        && block.cache_undo.is_none()
        && !block.cache_update_reversible
}

#[derive(Clone, Copy)]
struct CoordinateChange {
    position: usize,
    old_value: f64,
    proposed_value: f64,
}

#[inline]
fn switch_values_close(left: f64, right: f64) -> bool {
    let scale = 1.0 + left.abs().max(right.abs());
    (left - right).abs() <= 1e-9 * scale
}

fn build_switch_proposal(
    pair: &SwitchPair,
    theta: &[f64],
    posterior: &PosteriorLib,
) -> Option<Vec<f64>> {
    let current_j = posterior.switch_coordinate_value(pair.position_j, theta)?;
    let current_k = posterior.switch_coordinate_value(pair.position_k, theta)?;
    let (proposed_j, proposed_k) = pair.proposed_values(current_j, current_k)?;
    let coordinates = [pair.position_j, pair.position_k];
    let current_values = [current_j, current_k];
    let proposed_values = [proposed_j, proposed_k];

    // Ordinary coordinates and user-defined derived coordinates can be
    // disjoint or can have a setter that touches the other coordinate. Try
    // both deterministic assignment orders and retain the first one whose
    // getters exactly realize the two proposed switch coordinates.
    for order in [[0usize, 1usize], [1usize, 0usize]] {
        let mut proposed_theta = theta.to_vec();
        let mut ok = true;
        for which in order {
            if !posterior.assign_switch_coordinate(
                coordinates[which],
                current_values[which],
                proposed_values[which],
                &mut proposed_theta,
            ) {
                ok = false;
                break;
            }
        }
        if !ok {
            continue;
        }
        let Some(realized_j) =
            posterior.switch_coordinate_value(pair.position_j, &proposed_theta)
        else {
            continue;
        };
        let Some(realized_k) =
            posterior.switch_coordinate_value(pair.position_k, &proposed_theta)
        else {
            continue;
        };
        if switch_values_close(realized_j, proposed_j)
            && switch_values_close(realized_k, proposed_k)
        {
            return Some(proposed_theta);
        }
    }
    None
}

fn collect_coordinate_changes(
    theta: &[f64],
    proposed_theta: &[f64],
) -> Option<Vec<CoordinateChange>> {
    if theta.len() != proposed_theta.len() {
        return None;
    }
    let mut changes = Vec::new();
    for (position, (&old_value, &proposed_value)) in
        theta.iter().zip(proposed_theta.iter()).enumerate()
    {
        if !old_value.is_finite() || !proposed_value.is_finite() {
            return None;
        }
        if old_value.to_bits() != proposed_value.to_bits() {
            changes.push(CoordinateChange {
                position,
                old_value,
                proposed_value,
            });
        }
    }
    Some(changes)
}

fn rollback_block_switch(
    theta: &mut [f64],
    blocks: &[RuntimeBlock],
    coordinate_blocks: &[CoordinateBlockRef],
    changes: &[CoordinateChange],
    order: &[usize],
    staged_count: usize,
    used_snapshot: bool,
    posterior: &PosteriorLib,
) {
    if used_snapshot {
        posterior.cache_restore();
        for change_index in order[..staged_count].iter().copied() {
            let change = changes[change_index];
            theta[change.position] = change.old_value;
        }
        return;
    }

    for change_index in order[..staged_count].iter().rev().copied() {
        let change = changes[change_index];
        let coordinate = coordinate_blocks[change.position];
        let block = &blocks[coordinate.block_index];
        if let Some(undo_fn) = block.cache_undo {
            unsafe {
                undo_fn(
                    theta.as_ptr(),
                    coordinate.scalar_index,
                    change.old_value,
                );
            }
            theta[change.position] = change.old_value;
        } else if block.cache_update_reversible {
            theta[change.position] = change.old_value;
            if let Some(update_fn) = block.cache_update {
                unsafe {
                    update_fn(
                        theta.as_ptr(),
                        coordinate.scalar_index,
                        change.proposed_value,
                    );
                }
            }
        } else {
            theta[change.position] = change.old_value;
        }
    }
}

fn stage_block_switch_in_order(
    theta: &mut [f64],
    blocks: &[RuntimeBlock],
    coordinate_blocks: &[CoordinateBlockRef],
    changes: &[CoordinateChange],
    order: &[usize],
    posterior: &PosteriorLib,
) -> Option<(f64, bool)> {
    if changes.is_empty() || order.len() != changes.len() {
        return None;
    }
    let used_snapshot = order.iter().copied().any(|change_index| {
        let change = changes[change_index];
        let coordinate = coordinate_blocks[change.position];
        block_needs_snapshot_for_rejection(&blocks[coordinate.block_index])
    });
    if used_snapshot {
        posterior.cache_snapshot();
    }

    // Each scalar block-local difference is the exact full-target change for
    // that one-coordinate transition. The sum over an arbitrary derived
    // switch update therefore telescopes to log pi(T(theta)) - log pi(theta),
    // even when the derived setter changes many underlying parameters.
    let mut log_alpha = 0.0;
    let mut staged_count = 0usize;
    for change_index in order.iter().copied() {
        let change = changes[change_index];
        let coordinate = coordinate_blocks[change.position];
        let block = &blocks[coordinate.block_index];
        if block.value_kind != ValueKind::Continuous {
            rollback_block_switch(
                theta,
                blocks,
                coordinate_blocks,
                changes,
                order,
                staged_count,
                used_snapshot,
                posterior,
            );
            return None;
        }
        let current_local = unsafe { (block.f)(theta.as_ptr(), coordinate.scalar_index) };
        if !current_local.is_finite() {
            rollback_block_switch(
                theta,
                blocks,
                coordinate_blocks,
                changes,
                order,
                staged_count,
                used_snapshot,
                posterior,
            );
            return None;
        }

        theta[change.position] = change.proposed_value;
        if let Some(update_fn) = block.cache_update {
            unsafe {
                update_fn(
                    theta.as_ptr(),
                    coordinate.scalar_index,
                    change.old_value,
                );
            }
        }
        staged_count += 1;

        let proposed_local = unsafe { (block.f)(theta.as_ptr(), coordinate.scalar_index) };
        if !proposed_local.is_finite() {
            rollback_block_switch(
                theta,
                blocks,
                coordinate_blocks,
                changes,
                order,
                staged_count,
                used_snapshot,
                posterior,
            );
            return None;
        }
        log_alpha += proposed_local - current_local;
    }

    Some((log_alpha, used_snapshot))
}

fn attempt_block_switch(
    pair: &SwitchPair,
    theta: &mut [f64],
    current_logp: &mut f64,
    blocks: &[RuntimeBlock],
    coordinate_blocks: &[CoordinateBlockRef],
    posterior: &PosteriorLib,
    rng: &mut Xoshiro256StarStar,
) -> bool {
    let changes = if pair.position_j < theta.len() && pair.position_k < theta.len() {
        let current_j = theta[pair.position_j];
        let current_k = theta[pair.position_k];
        let Some((proposed_j, proposed_k)) = pair.proposed_values(current_j, current_k) else {
            return false;
        };
        let mut changes = Vec::with_capacity(2);
        if current_j.to_bits() != proposed_j.to_bits() {
            changes.push(CoordinateChange {
                position: pair.position_j,
                old_value: current_j,
                proposed_value: proposed_j,
            });
        }
        if current_k.to_bits() != proposed_k.to_bits() {
            changes.push(CoordinateChange {
                position: pair.position_k,
                old_value: current_k,
                proposed_value: proposed_k,
            });
        }
        changes
    } else {
        let Some(proposed_theta) = build_switch_proposal(pair, theta, posterior) else {
            return false;
        };
        let Some(changes) = collect_coordinate_changes(theta, &proposed_theta) else {
            return false;
        };
        changes
    };
    if changes.is_empty() {
        return false;
    }

    let mut selected_order: Vec<usize> = (0..changes.len()).collect();
    let mut transaction = stage_block_switch_in_order(
        theta,
        blocks,
        coordinate_blocks,
        &changes,
        &selected_order,
        posterior,
    );
    if transaction.is_none() {
        selected_order.reverse();
        transaction = stage_block_switch_in_order(
            theta,
            blocks,
            coordinate_blocks,
            &changes,
            &selected_order,
            posterior,
        );
    }
    let Some((log_alpha, used_snapshot)) = transaction else {
        return false;
    };

    let accepted = metropolis_accept(log_alpha, rng.uniform_open01());
    if accepted {
        *current_logp += log_alpha;
        true
    } else {
        rollback_block_switch(
            theta,
            blocks,
            coordinate_blocks,
            &changes,
            &selected_order,
            changes.len(),
            used_snapshot,
            posterior,
        );
        false
    }
}

fn attempt_full_switch(
    pair: &SwitchPair,
    theta: &mut [f64],
    current_logp: &mut f64,
    posterior: &mut PosteriorLib,
    rng: &mut Xoshiro256StarStar,
) -> bool {
    if pair.position_j < theta.len() && pair.position_k < theta.len() {
        let current_j = theta[pair.position_j];
        let current_k = theta[pair.position_k];
        let Some((proposed_j, proposed_k)) = pair.proposed_values(current_j, current_k) else {
            return false;
        };
        theta[pair.position_j] = proposed_j;
        theta[pair.position_k] = proposed_k;
        let proposed_logp = posterior.logp(theta);
        let accepted = proposed_logp.is_finite()
            && metropolis_accept(proposed_logp - *current_logp, rng.uniform_open01());
        if accepted {
            *current_logp = proposed_logp;
            true
        } else {
            theta[pair.position_j] = current_j;
            theta[pair.position_k] = current_k;
            false
        }
    } else {
        let Some(proposed_theta) = build_switch_proposal(pair, theta, posterior) else {
            return false;
        };
        let proposed_logp = posterior.logp(&proposed_theta);
        let accepted = proposed_logp.is_finite()
            && metropolis_accept(proposed_logp - *current_logp, rng.uniform_open01());
        if accepted {
            theta.copy_from_slice(&proposed_theta);
            *current_logp = proposed_logp;
            true
        } else {
            false
        }
    }
}

fn total_iterations(cfg: &Config) -> usize {
    let retained_iterations = cfg.samples.checked_mul(cfg.thin).unwrap_or_else(|| {
        eprintln!("--samples multiplied by --thin is too large");
        process::exit(2);
    });
    cfg.burnin
        .checked_add(retained_iterations)
        .unwrap_or_else(|| {
            eprintln!("--burnin plus retained iterations is too large");
            process::exit(2);
        })
}

fn run_block_sampler(
    cfg: &Config,
    posterior: &mut PosteriorLib,
    mut theta: Vec<f64>,
    initial_lp: Option<f64>,
) {
    let dim = cfg.dim;
    let blocks = posterior.load_blocks(&cfg.blocks).unwrap_or_else(|error| {
        eprintln!("failed to load block functions: {}", error);
        process::exit(1);
    });

    let mut covered = vec![false; dim];
    let mut coordinate_blocks = vec![
        CoordinateBlockRef {
            block_index: usize::MAX,
            scalar_index: 0,
        };
        dim
    ];
    for (block_index, block) in blocks.iter().enumerate() {
        let end = block.offset.checked_add(block.len).unwrap_or_else(|| {
            eprintln!("block {} offset/length overflow", block.name);
            process::exit(2);
        });
        if end > dim {
            eprintln!("block {} offset/length exceeds dim", block.name);
            process::exit(2);
        }
        for (local, position) in (block.offset..end).enumerate() {
            if covered[position] {
                eprintln!(
                    "parameter position {} is covered by more than one scalar block",
                    position + 1
                );
                process::exit(2);
            }
            covered[position] = true;
            coordinate_blocks[position] = CoordinateBlockRef {
                block_index,
                scalar_index: (local + 1) as c_int,
            };
        }
    }
    if let Some(position) = covered.iter().position(|value| !*value) {
        eprintln!(
            "parameter position {} has no scalar block update",
            position + 1
        );
        process::exit(2);
    }

    let mut value_kinds = vec![ValueKind::Continuous; dim];
    for block in &blocks {
        for value_kind in &mut value_kinds[block.offset..(block.offset + block.len)] {
            *value_kind = block.value_kind;
        }
    }
    for pair in &cfg.declared_switches {
        let j_is_continuous = pair.position_j >= dim
            || value_kinds[pair.position_j] == ValueKind::Continuous;
        let k_is_continuous = pair.position_k >= dim
            || value_kinds[pair.position_k] == ValueKind::Continuous;
        if !j_is_continuous || !k_is_continuous {
            eprintln!(
                "declared switch pair {}:{} must contain continuous ordinary/derived coordinates",
                pair.position_j + 1,
                pair.position_k + 1
            );
            process::exit(2);
        }
    }
    if cfg.switch_enabled
        && blocks.iter().any(|block| {
            block.value_kind == ValueKind::Continuous
                && block_needs_snapshot_for_rejection(block)
        })
        && !posterior.has_cache_snapshot_restore()
    {
        eprintln!(
            "--switch cannot safely stage a joint proposal because the model has a non-reversible cache update but does not export hobbs_cache_snapshot/hobbs_cache_restore"
        );
        process::exit(2);
    }

    // Initialize finite-state parameters at their declared lower bounds before
    // deterministic caches are constructed.
    for block in &blocks {
        if block.value_kind == ValueKind::Discrete {
            for position in block.offset..(block.offset + block.len) {
                theta[position] = block.lower as f64;
            }
        }
    }
    posterior.init_cache(&theta).unwrap_or_else(|error| {
        eprintln!("failed to initialize hobbs cache: {}", error);
        process::exit(1);
    });

    let total_iters = total_iterations(cfg);
    let adapt_until = cfg.adapt_until.unwrap_or(cfg.burnin).min(total_iters);
    let mut rng = Xoshiro256StarStar::new(cfg.seed);
    let mut retained_output = RetainedOutput::new(cfg, dim, total_iters, cfg.samples);
    let mut saved = 0usize;

    let mut proposal_scales = vec![cfg.step; dim];
    let mut tuning_factors = vec![1.0; dim];
    let mut covariance_shapes = vec![1.0; dim];
    let min_tuning = (-20.0f64).exp();
    let max_tuning = 20.0f64.exp();
    let min_scale = (cfg.step * min_tuning).max(1e-12);
    let max_scale = (cfg.step * max_tuning).min(1e12);
    // Every declared scalar coordinate is visited exactly once per sweep.
    // Proposal counts are therefore known without incrementing a hot counter
    // for every transition.
    let coordinate_proposals = vec![total_iters as u64; dim];
    let mut coordinate_accepts = vec![0u64; dim];
    let mut coordinate_adapt_proposals = vec![0u64; dim];
    let mut coordinate_adapt_accepts = vec![0u64; dim];
    let mut continuous_proposals = 0u64;
    let mut continuous_accepts = 0u64;
    let mut discrete_updates = 0u64;
    let mut discrete_moves = 0u64;

    let continuous_positions: Vec<usize> = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous)
        .flat_map(|block| block.offset..(block.offset + block.len))
        .collect();
    for &position in &continuous_positions {
        coordinate_adapt_proposals[position] = adapt_until as u64;
    }
    let geometry_schedule = GeometrySchedule::new(adapt_until);
    let mut covariance = CovarianceAdapter::new(
        cfg.covariance_mode,
        continuous_positions.clone(),
        cfg.covariance_max_dim,
        cfg.switch_enabled,
        cfg.switch_max_dim,
    );
    let mut declared_switch_trainer =
        DeclaredSwitchTrainer::new(cfg.declared_switches.clone());
    let mut switches = SwitchController::new(
        cfg.switch_enabled,
        cfg.switch_threshold,
        geometry_schedule,
    );

    let max_continuous_len = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous)
        .map(|block| block.len)
        .max()
        .unwrap_or(0);
    let max_discrete_values = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Discrete)
        .map(|block| discrete_state_count(block.lower, block.upper, &block.name))
        .max()
        .unwrap_or(0);
    let mut normals = vec![0.0f64; max_continuous_len];
    let mut uniforms = vec![0.0f64; max_continuous_len];
    let mut accepted_flags = vec![0u8; max_continuous_len];
    let mut discrete_logps = vec![f64::NEG_INFINITY; max_discrete_values];

    let mut current_logp = initial_lp.unwrap_or(0.0);
    let mut has_full_logp = initial_lp.is_some();
    if !has_full_logp && (posterior.has_scalar() || posterior.has_batch()) {
        let value = posterior.logp(&theta);
        if value.is_finite() {
            current_logp = value;
            has_full_logp = true;
        }
    }

    let generated_sweep_count = blocks
        .iter()
        .filter(|block| {
            block.value_kind == ValueKind::Continuous
                && (block.continuous_adaptive_sweep.is_some() || block.continuous_sweep.is_some())
        })
        .count();
    let fused_sweep_count = blocks
        .iter()
        .filter(|block| {
            block.value_kind == ValueKind::Continuous
                && block.len >= FUSED_ADAPTIVE_SWEEP_MIN_LEN
                && block.continuous_adaptive_sweep.is_some()
        })
        .count();
    let continuous_block_count = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous)
        .count();

    if !cfg.quiet {
        eprintln!("  update mode:          adaptive scalar Metropolis-within-Gibbs");
        eprintln!(
            "  block functions:      {}",
            blocks
                .iter()
                .map(|block| {
                    if block.value_kind == ValueKind::Discrete {
                        format!(
                            "{}:scalar:{}[{},{}]",
                            block.name,
                            block.value_kind.as_str(),
                            block.lower,
                            block.upper
                        )
                    } else {
                        format!("{}:scalar:{}", block.name, block.value_kind.as_str())
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        );
        eprintln!("  scalar parameters:    {}", dim);
        eprintln!(
            "  generated C sweeps:   {}/{} continuous blocks",
            generated_sweep_count, continuous_block_count
        );
        eprintln!(
            "  fused adapt sweeps:   {}/{} continuous blocks",
            fused_sweep_count, continuous_block_count
        );
        eprintln!(
            "  full posterior logp:  {}",
            if has_full_logp {
                "yes"
            } else {
                "no (reported logp is relative to the initial state)"
            }
        );
        eprintln!(
            "  attached cache:       {}",
            if posterior.has_cache() { "yes" } else { "no" }
        );
        eprintln!("  covariance adaptation:{}", covariance.name());
        if cfg.switch_enabled {
            eprintln!(
                "  correlation switches: enabled (one-time training {}..{}, |r| > {:.3})",
                geometry_schedule.collect_start,
                geometry_schedule.fit_iter,
                cfg.switch_threshold
            );
            eprintln!(
                "  declared switch pairs:{} (attempt every pair after every scalar sweep)",
                cfg.declared_switches.len()
            );
            eprintln!(
                "  automatic greedy:     {} (dense limit {})",
                if covariance.greedy_enabled() { "enabled" } else { "skipped" },
                cfg.switch_max_dim
            );
        }
    }

    let start = std::time::Instant::now();
    let mut last_progress_percent = usize::MAX;
    if !cfg.quiet {
        print_progress(0, total_iters, &mut last_progress_percent);
    }

    for iter in 1..=total_iters {
        let mut accepted_sweep = false;
        let adapting = iter <= adapt_until;
        let (scale_up, scale_down) = if adapting {
            adaptation_multipliers(iter, cfg.target_accept)
        } else {
            (1.0, 1.0)
        };

        for block in &blocks {
            if block.value_kind == ValueKind::Continuous {
                if block.len >= FUSED_ADAPTIVE_SWEEP_MIN_LEN {
                    if let Some(sweep_fn) = block.continuous_adaptive_sweep {
                        for local in 0..block.len {
                            normals[local] = rng.normal();
                            uniforms[local] = rng.uniform_open01();
                        }
                        let mut bad_index: c_int = 0;
                        let mut accepted_in_block = 0u64;
                        let delta_sum = unsafe {
                            sweep_fn(
                                theta.as_mut_ptr(),
                                proposal_scales.as_mut_ptr().add(block.offset),
                                tuning_factors.as_mut_ptr().add(block.offset),
                                covariance_shapes.as_ptr().add(block.offset),
                                normals.as_ptr(),
                                uniforms.as_ptr(),
                                coordinate_accepts.as_mut_ptr().add(block.offset),
                                coordinate_adapt_accepts.as_mut_ptr().add(block.offset),
                                adapting as c_int,
                                cfg.step,
                                scale_up,
                                scale_down,
                                min_tuning,
                                max_tuning,
                                min_scale,
                                max_scale,
                                &mut accepted_in_block,
                                &mut bad_index,
                            )
                        };
                        if delta_sum.is_nan() {
                            eprintln!(
                                "block {}({}) gave a non-finite current local log posterior",
                                block.name, bad_index
                            );
                            process::exit(1);
                        }
                        current_logp += delta_sum;
                        continuous_proposals += block.len as u64;
                        continuous_accepts += accepted_in_block;
                        if accepted_in_block > 0 {
                            accepted_sweep = true;
                        }
                        continue;
                    }
                }

                if let Some(sweep_fn) = block.continuous_sweep {
                    for local in 0..block.len {
                        normals[local] = rng.normal();
                        uniforms[local] = rng.uniform_open01();
                    }
                    let mut bad_index: c_int = 0;
                    let scale_slice = &proposal_scales[block.offset..(block.offset + block.len)];
                    let delta_sum = unsafe {
                        sweep_fn(
                            theta.as_mut_ptr(),
                            scale_slice.as_ptr(),
                            normals.as_ptr(),
                            uniforms.as_ptr(),
                            accepted_flags.as_mut_ptr(),
                            &mut bad_index,
                        )
                    };
                    if delta_sum.is_nan() {
                        eprintln!(
                            "block {}({}) gave a non-finite current local log posterior",
                            block.name, bad_index
                        );
                        process::exit(1);
                    }
                    current_logp += delta_sum;
                    for local in 0..block.len {
                        let position = block.offset + local;
                        let accepted = accepted_flags[local] != 0;
                        continuous_proposals += 1;
                        if accepted {
                            coordinate_accepts[position] += 1;
                            continuous_accepts += 1;
                            accepted_sweep = true;
                        }
                        if adapting {
                            if accepted {
                                coordinate_adapt_accepts[position] += 1;
                            }
                            adapt_tuning_factor(
                                &mut tuning_factors[position],
                                accepted,
                                scale_up,
                                scale_down,
                                min_tuning,
                                max_tuning,
                            );
                            proposal_scales[position] = compose_proposal_scale(
                                cfg.step,
                                tuning_factors[position],
                                covariance_shapes[position],
                                min_scale,
                                max_scale,
                            );
                        }
                    }
                    continue;
                }

                // Compatibility fallback for hand-written shared libraries.
                // Package-generated models use the one-call C sweep above.
                for local in 0..block.len {
                    let index = (local + 1) as c_int;
                    let position = block.offset + local;
                    let old_value = theta[position];
                    let old_local = unsafe { (block.f)(theta.as_ptr(), index) };
                    if !old_local.is_finite() {
                        eprintln!(
                            "block {}({}) gave non-finite current local logp: {}",
                            block.name, index, old_local
                        );
                        process::exit(1);
                    }
                    let proposed_value = old_value + proposal_scales[position] * rng.normal();
                    theta[position] = proposed_value;
                    if let Some(update_fn) = block.cache_update {
                        if block.cache_undo.is_none() && !block.cache_update_reversible {
                            posterior.cache_snapshot();
                        }
                        unsafe {
                            update_fn(theta.as_ptr(), index, old_value);
                        }
                    }
                    let new_local = unsafe { (block.f)(theta.as_ptr(), index) };
                    let log_alpha = new_local - old_local;
                    let accepted =
                        new_local.is_finite() && metropolis_accept(log_alpha, rng.uniform_open01());
                    if accepted {
                        current_logp += log_alpha;
                        coordinate_accepts[position] += 1;
                        continuous_accepts += 1;
                        accepted_sweep = true;
                    } else if block.cache_update.is_some() {
                        if let Some(undo_fn) = block.cache_undo {
                            // Generated algebraic undo functions expect theta to
                            // remain at the proposal and receive the original
                            // coordinate value.  This exactly replays the same
                            // branch/RHS and only reverses the cache writes.
                            unsafe {
                                undo_fn(theta.as_ptr(), index, old_value);
                            }
                            theta[position] = old_value;
                        } else if block.cache_update_reversible {
                            // Legacy hand-written ABI: reverse by swapping the
                            // old/proposed values before invoking cache_update.
                            theta[position] = old_value;
                            unsafe {
                                block.cache_update.unwrap()(theta.as_ptr(), index, proposed_value);
                            }
                        } else {
                            posterior.cache_restore();
                            theta[position] = old_value;
                        }
                    } else {
                        theta[position] = old_value;
                    }
                    continuous_proposals += 1;
                    if adapting {
                        if accepted {
                            coordinate_adapt_accepts[position] += 1;
                        }
                        adapt_tuning_factor(
                            &mut tuning_factors[position],
                            accepted,
                            scale_up,
                            scale_down,
                            min_tuning,
                            max_tuning,
                        );
                        proposal_scales[position] = compose_proposal_scale(
                            cfg.step,
                            tuning_factors[position],
                            covariance_shapes[position],
                            min_scale,
                            max_scale,
                        );
                    }
                }
                continue;
            }

            // Exact finite-state Gibbs update, one coordinate at a time.
            for local in 0..block.len {
                let index = (local + 1) as c_int;
                let position = block.offset + local;
                let old_value = theta[position];
                let old_integer = old_value.round() as i64;
                let old_local = unsafe { (block.f)(theta.as_ptr(), index) };
                let state_count = discrete_state_count(block.lower, block.upper, &block.name);
                let generated_transaction = block.scalar_candidate.is_some()
                    && block.scalar_accept.is_some()
                    && block.scalar_reject.is_some();
                let mut max_logp = f64::NEG_INFINITY;

                for (state_index, value) in (block.lower..=block.upper).enumerate() {
                    let candidate_logp = if value == old_integer {
                        old_local
                    } else if generated_transaction {
                        let candidate = block.scalar_candidate.unwrap();
                        let reject = block.scalar_reject.unwrap();
                        let value_logp = unsafe {
                            candidate(
                                theta.as_mut_ptr(),
                                index,
                                position as c_int,
                                old_value,
                                value as f64,
                            )
                        };
                        unsafe {
                            reject(
                                theta.as_mut_ptr(),
                                index,
                                position as c_int,
                                old_value,
                                value as f64,
                            );
                        }
                        value_logp
                    } else if let Some(update_fn) = block.cache_update {
                        theta[position] = value as f64;
                        if block.cache_undo.is_none() && !block.cache_update_reversible {
                            posterior.cache_snapshot();
                        }
                        unsafe {
                            update_fn(theta.as_ptr(), index, old_value);
                        }
                        let value_logp = unsafe { (block.f)(theta.as_ptr(), index) };
                        if let Some(undo_fn) = block.cache_undo {
                            unsafe {
                                undo_fn(theta.as_ptr(), index, old_value);
                            }
                            theta[position] = old_value;
                        } else if block.cache_update_reversible {
                            theta[position] = old_value;
                            unsafe {
                                update_fn(theta.as_ptr(), index, value as f64);
                            }
                        } else {
                            posterior.cache_restore();
                            theta[position] = old_value;
                        }
                        value_logp
                    } else {
                        theta[position] = value as f64;
                        let value_logp = unsafe { (block.f)(theta.as_ptr(), index) };
                        theta[position] = old_value;
                        value_logp
                    };
                    discrete_logps[state_index] = candidate_logp;
                    if candidate_logp > max_logp {
                        max_logp = candidate_logp;
                    }
                }

                if !max_logp.is_finite() {
                    eprintln!(
                        "discrete block {}({}) has no finite state",
                        block.name, index
                    );
                    process::exit(1);
                }
                let mut total_weight = 0.0f64;
                for &value_logp in &discrete_logps[..state_count] {
                    if value_logp.is_finite() {
                        total_weight += (value_logp - max_logp).exp();
                    }
                }
                if !(total_weight > 0.0) || !total_weight.is_finite() {
                    eprintln!(
                        "discrete block {}({}) produced invalid normalized weights",
                        block.name, index
                    );
                    process::exit(1);
                }

                let mut draw = rng.uniform_open01() * total_weight;
                // Default to the last state so a final ulp of normalization
                // error cannot incorrectly wrap the draw back to state zero.
                let mut chosen_state = state_count - 1;
                for (state_index, &value_logp) in discrete_logps[..state_count].iter().enumerate() {
                    let weight = if value_logp.is_finite() {
                        (value_logp - max_logp).exp()
                    } else {
                        0.0
                    };
                    if draw <= weight {
                        chosen_state = state_index;
                        break;
                    }
                    draw -= weight;
                }
                let chosen_value = block.lower + chosen_state as i64;
                let mut chosen_logp = discrete_logps[chosen_state];

                if chosen_value != old_integer {
                    if generated_transaction {
                        let candidate = block.scalar_candidate.unwrap();
                        let accept = block.scalar_accept.unwrap();
                        chosen_logp = unsafe {
                            candidate(
                                theta.as_mut_ptr(),
                                index,
                                position as c_int,
                                old_value,
                                chosen_value as f64,
                            )
                        };
                        if !chosen_logp.is_finite() {
                            eprintln!(
                                "discrete block {}({}) selected a state that became non-finite",
                                block.name, index
                            );
                            process::exit(1);
                        }
                        unsafe {
                            accept();
                        }
                    } else {
                        theta[position] = chosen_value as f64;
                        if let Some(update_fn) = block.cache_update {
                            unsafe {
                                update_fn(theta.as_ptr(), index, old_value);
                            }
                        }
                    }
                    discrete_moves += 1;
                    coordinate_accepts[position] += 1;
                    accepted_sweep = true;
                } else {
                    theta[position] = old_value;
                }
                discrete_updates += 1;

                if old_local.is_finite() && chosen_logp.is_finite() {
                    current_logp += chosen_logp - old_local;
                } else if has_full_logp {
                    let value = posterior.logp(&theta);
                    if value.is_finite() {
                        current_logp = value;
                    }
                } else {
                    current_logp = chosen_logp;
                }
            }
        }

        if adapting && geometry_schedule.should_collect(iter) {
            covariance.update(&theta);
            if cfg.switch_enabled {
                declared_switch_trainer.update(&theta, posterior);
            }
        }
        if adapting && iter == geometry_schedule.fit_iter {
            if covariance.fit_shapes_once(&mut covariance_shapes) {
                rebuild_proposal_scales(
                    &continuous_positions,
                    cfg.step,
                    &tuning_factors,
                    &covariance_shapes,
                    &mut proposal_scales,
                    min_scale,
                    max_scale,
                );
            }
            if cfg.switch_enabled {
                let declared_pairs = declared_switch_trainer.pairs();
                let greedy_pairs = covariance.switch_pairs(switches.threshold);
                switches.activate(declared_pairs, greedy_pairs);
            }
        }

        if cfg.switch_enabled && switches.active && !switches.pairs.is_empty() {
            // Every declared pair is a separate invariant MH kernel. Applying
            // them sequentially permits overlapping cycles such as
            // beta(1)~u(1,1), ..., beta(1)~u(m,1) without violating Markovness.
            for pair_index in 0..switches.pairs.len() {
                let accepted = {
                    let pair = &switches.pairs[pair_index];
                    attempt_block_switch(
                        pair,
                        &mut theta,
                        &mut current_logp,
                        &blocks,
                        &coordinate_blocks,
                        posterior,
                        &mut rng,
                    )
                };
                switches.record(pair_index, accepted, iter <= cfg.burnin);
                if accepted {
                    accepted_sweep = true;
                }
            }
        }

        if iter > cfg.burnin && ((iter - cfg.burnin) % cfg.thin == 0) {
            saved += 1;
            retained_output.retain(iter as u64, current_logp, accepted_sweep, &theta);
        }
        if !cfg.quiet {
            print_progress(iter, total_iters, &mut last_progress_percent);
        }
    }
    if !cfg.quiet {
        eprintln!();
    }

    retained_output.finish(total_iters);

    if let Some(path) = cfg.adapt_diagnostics_out.as_deref() {
        write_adaptation_diagnostics(
            path,
            &value_kinds,
            &coordinate_proposals,
            &coordinate_accepts,
            &coordinate_adapt_proposals,
            &coordinate_adapt_accepts,
            &proposal_scales,
            &tuning_factors,
            &covariance_shapes,
            &covariance,
        );
    }
    if let Some(path) = cfg.adapt_covariance_out.as_deref() {
        covariance.write_covariance_diagnostics(
            path,
            geometry_schedule.collect_start,
            geometry_schedule.fit_iter,
        );
    }
    if let Some(path) = cfg.switch_diagnostics_out.as_deref() {
        switches.write_diagnostics(path);
    }

    if !cfg.quiet {
        let seconds = start.elapsed().as_secs_f64();
        let covariance_summary = covariance.summary();
        let total_updates = continuous_proposals + discrete_updates;
        eprintln!("done");
        eprintln!("  dim:                  {}", dim);
        eprintln!("  sweeps:               {}", total_iters);
        eprintln!("  scalar updates:       {}", total_updates);
        eprintln!("  saved samples:        {}", saved);

        if !continuous_positions.is_empty() {
            let mut scale_min = f64::INFINITY;
            let mut scale_max = f64::NEG_INFINITY;
            let mut scale_sum = 0.0;
            let mut rate_min = f64::INFINITY;
            let mut rate_max = f64::NEG_INFINITY;
            let mut rate_sum = 0.0;
            for &position in &continuous_positions {
                let scale = proposal_scales[position];
                scale_min = scale_min.min(scale);
                scale_max = scale_max.max(scale);
                scale_sum += scale;
                let rate = coordinate_accepts[position] as f64
                    / (coordinate_proposals[position] as f64).max(1.0);
                rate_min = rate_min.min(rate);
                rate_max = rate_max.max(rate);
                rate_sum += rate;
            }
            let count = continuous_positions.len() as f64;
            eprintln!(
                "  continuous accept:    {:.4}",
                continuous_accepts as f64 / continuous_proposals.max(1) as f64
            );
            eprintln!(
                "  per-coordinate rate:  mean {:.4}, min {:.4}, max {:.4}",
                rate_sum / count,
                rate_min,
                rate_max
            );
            eprintln!(
                "  proposal scale:       mean {:.6e}, min {:.6e}, max {:.6e}",
                scale_sum / count,
                scale_min,
                scale_max
            );
            eprintln!("  target accept:        {:.4}", cfg.target_accept);
        }
        if discrete_updates > 0 {
            eprintln!(
                "  discrete move rate:   {:.4}",
                discrete_moves as f64 / discrete_updates as f64
            );
        }
        eprintln!("  adapt until:          {}", adapt_until);
        eprintln!(
            "  covariance adaptation:{} ({} sweep samples)",
            covariance.name(),
            covariance_summary.samples
        );
        if covariance_summary.samples >= 2 {
            eprintln!(
                "  marginal variance:    mean {:.6e}, min {:.6e}, max {:.6e}",
                covariance_summary.mean_variance,
                covariance_summary.min_variance,
                covariance_summary.max_variance
            );
            if let (Some(mean_sd), Some(min_sd), Some(max_sd)) = (
                covariance_summary.mean_conditional_sd,
                covariance_summary.min_conditional_sd,
                covariance_summary.max_conditional_sd,
            ) {
                eprintln!(
                    "  learned scalar shape: mean {:.6e}, min {:.6e}, max {:.6e}",
                    mean_sd, min_sd, max_sd
                );
            }
            if let Some(max_correlation) = covariance_summary.max_abs_correlation {
                eprintln!("  max |correlation|:    {:.6}", max_correlation);
            }
        }
        if cfg.switch_enabled {
            let warmup_attempts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.warmup_attempts)
                .sum();
            let warmup_accepts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.warmup_accepts)
                .sum();
            let sampling_attempts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.sampling_attempts)
                .sum();
            let sampling_accepts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.sampling_accepts)
                .sum();
            let declared_count = switches
                .pairs
                .iter()
                .filter(|pair| matches!(pair.source, SwitchSource::Declared(_)))
                .count();
            let greedy_count = switches.pairs.len() - declared_count;
            eprintln!(
                "  frozen switch pairs:  {} ({} declared, {} greedy)",
                switches.pairs.len(),
                declared_count,
                greedy_count
            );
            eprintln!(
                "  warmup switch rate:     {} attempts, {:.4} accepted",
                warmup_attempts,
                warmup_accepts as f64 / warmup_attempts.max(1) as f64
            );
            eprintln!(
                "  sampling switch rate:  {} attempts, {:.4} accepted",
                sampling_attempts,
                sampling_accepts as f64 / sampling_attempts.max(1) as f64
            );
        }
        eprintln!("  seconds:              {:.6}", seconds);
        eprintln!(
            "  sweeps/sec:           {:.0}",
            total_iters as f64 / seconds.max(1e-12)
        );
        eprintln!(
            "  scalar updates/sec:   {:.0}",
            total_updates as f64 / seconds.max(1e-12)
        );
        eprintln!(
            "  output:               {}",
            retained_output.description(cfg)
        );
    }
}

fn run_full_scalar_sampler(
    cfg: &Config,
    posterior: &mut PosteriorLib,
    mut theta: Vec<f64>,
    mut current_logp: f64,
) {
    let dim = cfg.dim;
    let total_iters = total_iterations(cfg);
    let adapt_until = cfg.adapt_until.unwrap_or(cfg.burnin).min(total_iters);
    let mut retained_output = RetainedOutput::new(cfg, dim, total_iters, cfg.samples);
    let mut rng = Xoshiro256StarStar::new(cfg.seed);
    let mut proposal_scales = vec![cfg.step; dim];
    let mut tuning_factors = vec![1.0; dim];
    let mut covariance_shapes = vec![1.0; dim];
    let min_tuning = (-20.0f64).exp();
    let max_tuning = 20.0f64.exp();
    let min_scale = (cfg.step * min_tuning).max(1e-12);
    let max_scale = (cfg.step * max_tuning).min(1e12);
    let mut proposals = vec![0u64; dim];
    let mut accepts = vec![0u64; dim];
    let mut adapt_proposals = vec![0u64; dim];
    let mut adapt_accepts = vec![0u64; dim];
    let positions: Vec<usize> = (0..dim).collect();
    let geometry_schedule = GeometrySchedule::new(adapt_until);
    let mut covariance = CovarianceAdapter::new(
        cfg.covariance_mode,
        positions.clone(),
        cfg.covariance_max_dim,
        cfg.switch_enabled,
        cfg.switch_max_dim,
    );
    let mut declared_switch_trainer =
        DeclaredSwitchTrainer::new(cfg.declared_switches.clone());
    let mut switches = SwitchController::new(
        cfg.switch_enabled,
        cfg.switch_threshold,
        geometry_schedule,
    );
    let mut accepted_total = 0u64;
    let mut saved = 0usize;

    if !cfg.quiet {
        eprintln!("  update mode:          full-target adaptive scalar Metropolis-within-Gibbs");
        eprintln!("  scalar parameters:    {}", dim);
        eprintln!("  covariance adaptation:{}", covariance.name());
        if cfg.switch_enabled {
            eprintln!(
                "  correlation switches: enabled (one-time training {}..{}, |r| > {:.3})",
                geometry_schedule.collect_start,
                geometry_schedule.fit_iter,
                cfg.switch_threshold
            );
            eprintln!(
                "  declared switch pairs:{} (attempt every pair after every scalar sweep)",
                cfg.declared_switches.len()
            );
            eprintln!(
                "  automatic greedy:     {} (dense limit {})",
                if covariance.greedy_enabled() { "enabled" } else { "skipped" },
                cfg.switch_max_dim
            );
        }
    }

    let start = std::time::Instant::now();
    let mut last_progress_percent = usize::MAX;
    if !cfg.quiet {
        print_progress(0, total_iters, &mut last_progress_percent);
    }

    for iter in 1..=total_iters {
        let adapting = iter <= adapt_until;
        let (scale_up, scale_down) = if adapting {
            adaptation_multipliers(iter, cfg.target_accept)
        } else {
            (1.0, 1.0)
        };
        let mut accepted_sweep = false;

        for position in 0..dim {
            let old_value = theta[position];
            theta[position] = old_value + proposal_scales[position] * rng.normal();
            let proposed_logp = posterior.logp(&theta);
            let log_alpha = proposed_logp - current_logp;
            let accepted =
                proposed_logp.is_finite() && metropolis_accept(log_alpha, rng.uniform_open01());
            proposals[position] += 1;
            if accepted {
                current_logp = proposed_logp;
                accepts[position] += 1;
                accepted_total += 1;
                accepted_sweep = true;
            } else {
                theta[position] = old_value;
            }
            if adapting {
                adapt_proposals[position] += 1;
                if accepted {
                    adapt_accepts[position] += 1;
                }
                adapt_tuning_factor(
                    &mut tuning_factors[position],
                    accepted,
                    scale_up,
                    scale_down,
                    min_tuning,
                    max_tuning,
                );
                proposal_scales[position] = compose_proposal_scale(
                    cfg.step,
                    tuning_factors[position],
                    covariance_shapes[position],
                    min_scale,
                    max_scale,
                );
            }
        }

        if adapting && geometry_schedule.should_collect(iter) {
            covariance.update(&theta);
            if cfg.switch_enabled {
                declared_switch_trainer.update(&theta, posterior);
            }
        }
        if adapting && iter == geometry_schedule.fit_iter {
            if covariance.fit_shapes_once(&mut covariance_shapes) {
                rebuild_proposal_scales(
                    &positions,
                    cfg.step,
                    &tuning_factors,
                    &covariance_shapes,
                    &mut proposal_scales,
                    min_scale,
                    max_scale,
                );
            }
            if cfg.switch_enabled {
                let declared_pairs = declared_switch_trainer.pairs();
                let greedy_pairs = covariance.switch_pairs(switches.threshold);
                switches.activate(declared_pairs, greedy_pairs);
            }
        }

        if cfg.switch_enabled && switches.active && !switches.pairs.is_empty() {
            for pair_index in 0..switches.pairs.len() {
                let accepted = {
                    let pair = &switches.pairs[pair_index];
                    attempt_full_switch(
                        pair,
                        &mut theta,
                        &mut current_logp,
                        posterior,
                        &mut rng,
                    )
                };
                switches.record(pair_index, accepted, iter <= cfg.burnin);
                if accepted {
                    accepted_sweep = true;
                }
            }
        }

        if iter > cfg.burnin && ((iter - cfg.burnin) % cfg.thin == 0) {
            saved += 1;
            retained_output.retain(iter as u64, current_logp, accepted_sweep, &theta);
        }
        if !cfg.quiet {
            print_progress(iter, total_iters, &mut last_progress_percent);
        }
    }
    if !cfg.quiet {
        eprintln!();
    }

    retained_output.finish(total_iters);

    if let Some(path) = cfg.adapt_diagnostics_out.as_deref() {
        let value_kinds = vec![ValueKind::Continuous; dim];
        write_adaptation_diagnostics(
            path,
            &value_kinds,
            &proposals,
            &accepts,
            &adapt_proposals,
            &adapt_accepts,
            &proposal_scales,
            &tuning_factors,
            &covariance_shapes,
            &covariance,
        );
    }
    if let Some(path) = cfg.adapt_covariance_out.as_deref() {
        covariance.write_covariance_diagnostics(
            path,
            geometry_schedule.collect_start,
            geometry_schedule.fit_iter,
        );
    }
    if let Some(path) = cfg.switch_diagnostics_out.as_deref() {
        switches.write_diagnostics(path);
    }

    if !cfg.quiet {
        let seconds = start.elapsed().as_secs_f64();
        let mut scale_min = f64::INFINITY;
        let mut scale_max = f64::NEG_INFINITY;
        let mut scale_sum = 0.0;
        let mut rate_min = f64::INFINITY;
        let mut rate_max = f64::NEG_INFINITY;
        let mut rate_sum = 0.0;
        for position in 0..dim {
            scale_min = scale_min.min(proposal_scales[position]);
            scale_max = scale_max.max(proposal_scales[position]);
            scale_sum += proposal_scales[position];
            let rate = accepts[position] as f64 / (proposals[position] as f64).max(1.0);
            rate_min = rate_min.min(rate);
            rate_max = rate_max.max(rate);
            rate_sum += rate;
        }
        let total_proposals = total_iters as u64 * dim as u64;
        let covariance_summary = covariance.summary();
        eprintln!("done");
        eprintln!("  dim:                  {}", dim);
        eprintln!("  sweeps:               {}", total_iters);
        eprintln!("  scalar updates:       {}", total_proposals);
        eprintln!("  saved samples:        {}", saved);
        eprintln!(
            "  continuous accept:    {:.4}",
            accepted_total as f64 / total_proposals.max(1) as f64
        );
        eprintln!(
            "  per-coordinate rate:  mean {:.4}, min {:.4}, max {:.4}",
            rate_sum / dim as f64,
            rate_min,
            rate_max
        );
        eprintln!(
            "  proposal scale:       mean {:.6e}, min {:.6e}, max {:.6e}",
            scale_sum / dim as f64,
            scale_min,
            scale_max
        );
        eprintln!("  target accept:        {:.4}", cfg.target_accept);
        eprintln!("  adapt until:          {}", adapt_until);
        eprintln!(
            "  covariance adaptation:{} ({} sweep samples)",
            covariance.name(),
            covariance_summary.samples
        );
        if covariance_summary.samples >= 2 {
            eprintln!(
                "  marginal variance:    mean {:.6e}, min {:.6e}, max {:.6e}",
                covariance_summary.mean_variance,
                covariance_summary.min_variance,
                covariance_summary.max_variance
            );
            if let (Some(mean_sd), Some(min_sd), Some(max_sd)) = (
                covariance_summary.mean_conditional_sd,
                covariance_summary.min_conditional_sd,
                covariance_summary.max_conditional_sd,
            ) {
                eprintln!(
                    "  learned scalar shape: mean {:.6e}, min {:.6e}, max {:.6e}",
                    mean_sd, min_sd, max_sd
                );
            }
            if let Some(max_correlation) = covariance_summary.max_abs_correlation {
                eprintln!("  max |correlation|:    {:.6}", max_correlation);
            }
        }
        if cfg.switch_enabled {
            let warmup_attempts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.warmup_attempts)
                .sum();
            let warmup_accepts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.warmup_accepts)
                .sum();
            let sampling_attempts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.sampling_attempts)
                .sum();
            let sampling_accepts: u64 = switches
                .pairs
                .iter()
                .map(|pair| pair.sampling_accepts)
                .sum();
            let declared_count = switches
                .pairs
                .iter()
                .filter(|pair| matches!(pair.source, SwitchSource::Declared(_)))
                .count();
            let greedy_count = switches.pairs.len() - declared_count;
            eprintln!(
                "  frozen switch pairs:  {} ({} declared, {} greedy)",
                switches.pairs.len(),
                declared_count,
                greedy_count
            );
            eprintln!(
                "  warmup switch rate:     {} attempts, {:.4} accepted",
                warmup_attempts,
                warmup_accepts as f64 / warmup_attempts.max(1) as f64
            );
            eprintln!(
                "  sampling switch rate:  {} attempts, {:.4} accepted",
                sampling_attempts,
                sampling_accepts as f64 / sampling_attempts.max(1) as f64
            );
        }
        eprintln!("  seconds:              {:.6}", seconds);
        eprintln!(
            "  sweeps/sec:           {:.0}",
            total_iters as f64 / seconds.max(1e-12)
        );
        eprintln!(
            "  scalar updates/sec:   {:.0}",
            total_proposals as f64 / seconds.max(1e-12)
        );
        eprintln!(
            "  output:               {}",
            retained_output.description(cfg)
        );
    }
}

fn main() {
    let cfg = parse_args();
    let mut posterior = PosteriorLib::open(
        &cfg.lib,
        cfg.eval_mode,
        cfg.update_mode == UpdateMode::Block,
        cfg.switch_derived_count,
    )
    .unwrap_or_else(|e| {
        eprintln!("failed to load posterior: {}", e);
        process::exit(1);
    });
    posterior
        .init_with_data(cfg.data.as_deref())
        .unwrap_or_else(|e| {
            eprintln!("failed to initialize posterior: {}", e);
            process::exit(1);
        });

    if !cfg.quiet {
        eprintln!("posterior symbols:");
        eprintln!(
            "  posterior_logp:       {}",
            if posterior.has_scalar() { "yes" } else { "no" }
        );
        eprintln!(
            "  posterior_logp_batch: {}",
            if posterior.has_batch() { "yes" } else { "no" }
        );
        eprintln!(
            "  posterior_init:       {}",
            if posterior.has_init() { "yes" } else { "no" }
        );
        eprintln!(
            "  posterior_free:       {}",
            if posterior.has_free() { "yes" } else { "no" }
        );
        eprintln!(
            "  hobbs_cache_init:     {}",
            if posterior.has_cache() { "yes" } else { "no" }
        );
        eprintln!("  active eval mode:     {}", posterior.active_mode_name());
        eprintln!(
            "  derived switch coords:{}",
            posterior.switch_derived_count()
        );
    }

    let theta = vec![0.0; cfg.dim];
    if cfg.update_mode == UpdateMode::Block {
        let has_discrete = cfg
            .blocks
            .iter()
            .any(|b| b.value_kind == ValueKind::Discrete);
        let initial_lp = if (posterior.has_scalar() || posterior.has_batch())
            && !has_discrete
            && !posterior.has_cache()
        {
            let value = posterior.logp(&theta);
            if value.is_finite() {
                Some(value)
            } else {
                None
            }
        } else {
            None
        };
        run_block_sampler(&cfg, &mut posterior, theta, initial_lp);
        return;
    }

    if !(posterior.has_scalar() || posterior.has_batch()) {
        eprintln!("full-posterior scalar updates require posterior_logp or posterior_logp_batch");
        process::exit(1);
    }
    if posterior.has_cache() {
        eprintln!("attached deterministic caches require block/indexed updates so the cache can be maintained one scalar at a time");
        process::exit(1);
    }
    let initial_lp = posterior.logp(&theta);
    if !initial_lp.is_finite() {
        eprintln!(
            "initial theta = zeros gives non-finite log posterior: {}",
            initial_lp
        );
        eprintln!("edit the C model so zero is valid, or use indexed discrete blocks to construct a valid initial state");
        process::exit(1);
    }
    run_full_scalar_sampler(&cfg, &mut posterior, theta, initial_lp);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ziggurat_normal_has_expected_moments() {
        let mut rng = Xoshiro256StarStar::new(0x5eed_1234_9876_abcd);
        let n = 1_000_000usize;
        let mut sum = 0.0;
        let mut sumsq = 0.0;
        let mut positive = 0usize;
        let mut tail3 = 0usize;
        for _ in 0..n {
            let value = rng.normal();
            assert!(value.is_finite());
            sum += value;
            sumsq += value * value;
            positive += usize::from(value > 0.0);
            tail3 += usize::from(value.abs() > 3.0);
        }
        let mean = sum / n as f64;
        let variance = sumsq / n as f64 - mean * mean;
        let positive_rate = positive as f64 / n as f64;
        let tail_rate = tail3 as f64 / n as f64;
        assert!(mean.abs() < 0.005, "mean={mean}");
        assert!((variance - 1.0).abs() < 0.01, "variance={variance}");
        assert!(
            (positive_rate - 0.5).abs() < 0.005,
            "positive={positive_rate}"
        );
        assert!((0.0023..0.0031).contains(&tail_rate), "tail3={tail_rate}");
    }

    #[test]
    fn packed_covariance_recovers_full_correlation_and_conditional_scale() {
        let positions = [0usize, 1usize];
        let rho = 0.9f64;
        let orthogonal_weight = (1.0 - rho * rho).sqrt();
        let z1 = [-1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0];
        let z2 = [-1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0];
        let mut covariance = WarmupPackedMoments::new(2);
        for _ in 0..1_000 {
            for sample in 0..z1.len() {
                let theta = [
                    z1[sample],
                    rho * z1[sample] + orthogonal_weight * z2[sample],
                ];
                covariance.update(&theta, &positions);
            }
        }
        let variances = covariance.variances();
        assert!((variances[0] - 1.0).abs() < 0.001, "v0={}", variances[0]);
        assert!((variances[1] - 1.0).abs() < 0.001, "v1={}", variances[1]);
        let mut shapes = vec![0.0; 2];
        assert!(covariance.fill_shape_suggestions(&mut shapes));
        let expected = (1.0 - rho * rho).sqrt();
        assert!((shapes[0] - expected).abs() < 0.01, "s0={}", shapes[0]);
        assert!((shapes[1] - expected).abs() < 0.01, "s1={}", shapes[1]);
        let summary = covariance.summary();
        assert!((summary.max_abs_correlation.expect("correlation") - rho).abs() < 0.001);
    }

    #[test]
    fn centered_switches_are_involutions_for_both_correlation_signs() {
        for correlation in [-0.95, 0.95] {
            let pair = SwitchPair::new(
                0,
                1,
                1.25,
                -0.75,
                2.0,
                0.5,
                correlation,
                SwitchSource::Greedy,
            );
            let original = [3.5, -0.10];
            let (first_j, first_k) = pair
                .proposed_values(original[0], original[1])
                .expect("finite first switch");
            let switched = [first_j, first_k];
            let (second_j, second_k) = pair
                .proposed_values(switched[0], switched[1])
                .expect("finite second switch");
            assert!((second_j - original[0]).abs() < 1e-14);
            assert!((second_k - original[1]).abs() < 1e-14);
        }
    }

    #[test]
    fn geometry_schedule_fits_once_at_two_fifths_of_adaptive_warmup() {
        let schedule = GeometrySchedule::new(100);
        assert_eq!(schedule.collect_start, 20);
        assert_eq!(schedule.fit_iter, 40);
        assert!(!schedule.should_collect(19));
        assert!(schedule.should_collect(20));
        assert!(schedule.should_collect(40));
        assert!(!schedule.should_collect(41));

        let no_adaptation = GeometrySchedule::new(0);
        assert_eq!(no_adaptation.collect_start, 0);
        assert_eq!(no_adaptation.fit_iter, 0);
        assert!(!no_adaptation.should_collect(0));
        assert!(!no_adaptation.should_collect(1));
    }

    #[test]
    fn sparse_declared_trainer_reuses_coordinate_moments_and_recovers_edges() {
        let specs = vec![
            DeclaredSwitchSpec {
                position_j: 0,
                position_k: 1,
            },
            DeclaredSwitchSpec {
                position_j: 0,
                position_k: 2,
            },
        ];
        let mut trainer = DeclaredSwitchTrainer::new(specs);
        assert_eq!(trainer.unique_positions, vec![0, 1, 2]);
        assert_eq!(trainer.edge_locals, vec![(0, 1), (0, 2)]);
        for x in -100..=100 {
            let value = x as f64;
            trainer.update_values(&[value, 3.0 - 2.0 * value, -4.0 + 0.5 * value]);
        }
        let pairs = trainer.pairs();
        assert_eq!(pairs.len(), 2);
        assert_eq!(pairs[0].source, SwitchSource::Declared(1));
        assert_eq!(pairs[1].source, SwitchSource::Declared(2));
        assert!(pairs[0].center_j.abs() < 1e-14);
        assert!((pairs[0].center_k - 3.0).abs() < 1e-14);
        assert!((pairs[0].correlation + 1.0).abs() < 1e-12);
        assert!((pairs[0].sd_k / pairs[0].sd_j - 2.0).abs() < 1e-12);
        assert!((pairs[1].center_k + 4.0).abs() < 1e-14);
        assert!((pairs[1].correlation - 1.0).abs() < 1e-12);
        assert!((pairs[1].sd_k / pairs[1].sd_j - 0.5).abs() < 1e-12);
    }

    #[test]
    fn declared_cycle_order_is_preserved_and_only_exact_greedy_duplicates_are_removed() {
        let declared = vec![
            SwitchPair::new(
                0,
                1,
                0.0,
                0.0,
                1.0,
                1.0,
                -0.9,
                SwitchSource::Declared(1),
            ),
            SwitchPair::new(
                0,
                2,
                0.0,
                0.0,
                1.0,
                1.0,
                -0.8,
                SwitchSource::Declared(2),
            ),
        ];
        let greedy = vec![
            SwitchPair::new(
                1,
                0,
                0.0,
                0.0,
                1.0,
                1.0,
                -0.95,
                SwitchSource::Greedy,
            ),
            SwitchPair::new(
                2,
                3,
                0.0,
                0.0,
                1.0,
                1.0,
                0.92,
                SwitchSource::Greedy,
            ),
        ];
        let pairs = combine_switch_pairs(declared, greedy);
        assert_eq!(pairs.len(), 3);
        assert_eq!((pairs[0].position_j, pairs[0].position_k), (0, 1));
        assert_eq!((pairs[1].position_j, pairs[1].position_k), (0, 2));
        assert_eq!((pairs[2].position_j, pairs[2].position_k), (2, 3));
        assert_eq!(pairs[0].source, SwitchSource::Declared(1));
        assert_eq!(pairs[1].source, SwitchSource::Declared(2));
        assert_eq!(pairs[2].source, SwitchSource::Greedy);
    }

    #[test]
    fn greedy_switch_pairing_selects_strongest_disjoint_correlations() {
        let positions = [0usize, 1usize, 2usize, 3usize, 4usize];
        let u = [-1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0];
        let v = [-1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0];
        let w = [-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0];
        let rho_a = -0.98f64;
        let rho_b = 0.93f64;
        let mut covariance = WarmupPackedMoments::new(positions.len());
        for _ in 0..1_000 {
            for sample in 0..u.len() {
                let theta = [
                    u[sample],
                    rho_a * u[sample] + (1.0 - rho_a * rho_a).sqrt() * v[sample],
                    v[sample],
                    rho_b * v[sample] + (1.0 - rho_b * rho_b).sqrt() * w[sample],
                    w[sample],
                ];
                covariance.update(&theta, &positions);
            }
        }
        let pairs = covariance.greedy_switch_pairs(&positions, 0.8);
        assert_eq!(pairs.len(), 2);
        assert_eq!((pairs[0].position_j, pairs[0].position_k), (0, 1));
        assert!(pairs[0].correlation < -0.97);
        assert_eq!((pairs[1].position_j, pairs[1].position_k), (2, 3));
        assert!(pairs[1].correlation > 0.92);
        assert!(pairs.iter().all(|pair| pair.position_j != pair.position_k));
    }

    #[test]
    fn robbins_monro_update_is_coordinate_local_per_sweep() {
        let target = 0.44;
        let mut factor = 1.0;
        let mut expected_log = 0.0;
        let outcomes = [true, false, true, true, false, false, true];
        for (index, accepted) in outcomes.into_iter().enumerate() {
            let sweep = index + 1;
            let (up, down) = adaptation_multipliers(sweep, target);
            adapt_tuning_factor(&mut factor, accepted, up, down, 1e-30, 1e30);
            let gain = ((sweep as f64) + 10.0).powf(-0.6);
            expected_log += gain * (f64::from(accepted) - target);
        }
        assert!((factor.ln() - expected_log).abs() < 1e-14);
    }

    #[test]
    fn declaration_save_ranges_are_sorted_merged_and_complemented() {
        let mut ranges = vec![
            SaveRange { offset: 7, len: 2 },
            SaveRange { offset: 2, len: 3 },
            SaveRange { offset: 5, len: 2 },
        ];
        normalize_save_ranges(&mut ranges, 12);
        assert_eq!(ranges, vec![SaveRange { offset: 2, len: 7 },]);
        assert_eq!(
            complement_save_ranges(12, &ranges),
            vec![
                SaveRange { offset: 0, len: 2 },
                SaveRange { offset: 9, len: 3 },
            ]
        );
    }

    #[test]
    fn selected_mean_accumulator_uses_declared_parameter_order() {
        let ranges = [
            SaveRange { offset: 1, len: 2 },
            SaveRange { offset: 4, len: 1 },
        ];
        let mut accumulator = SelectedMeanAccumulator::new(&ranges);
        accumulator.update(-3.0, &[1.0, 2.0, 3.0, 4.0, 5.0]);
        accumulator.update(-1.0, &[11.0, 12.0, 13.0, 14.0, 15.0]);
        assert_eq!(accumulator.n, 2);
        assert_eq!(accumulator.theta_sum, vec![14.0, 16.0, 20.0]);
        assert_eq!(accumulator.logp_sum, -4.0);
    }

    #[test]
    fn selected_mean_binary_is_one_standard_hobbs_record() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let ranges = [
            SaveRange { offset: 1, len: 2 },
            SaveRange { offset: 4, len: 1 },
        ];
        let mut accumulator = SelectedMeanAccumulator::new(&ranges);
        accumulator.update(-3.0, &[1.0, 2.0, 3.0, 4.0, 5.0]);
        accumulator.update(-1.0, &[11.0, 12.0, 13.0, 14.0, 15.0]);

        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!(
            "hobbs-selected-mean-{}-{unique}.bin",
            process::id()
        ));
        accumulator.write_binary(path.to_str().expect("utf-8 temp path"), 25);

        let bytes = fs::read(&path).expect("read selected mean binary");
        fs::remove_file(&path).expect("remove selected mean binary");
        assert_eq!(&bytes[..16], b"hobbs_BIN_V1\0\0\0\0");
        let u64_at = |offset: usize| {
            u64::from_le_bytes(bytes[offset..offset + 8].try_into().expect("u64 bytes"))
        };
        let f64_at = |offset: usize| {
            f64::from_le_bytes(bytes[offset..offset + 8].try_into().expect("f64 bytes"))
        };
        assert_eq!(u64_at(16), 1);
        assert_eq!(u64_at(24), 3);
        assert_eq!(u64_at(32), 25);
        assert_eq!(u64_at(40), 1);
        assert_eq!(u64_at(48), 48);
        assert_eq!(bytes.len(), 56 + 48);
        assert_eq!(u64_at(56), 2);
        assert_eq!(bytes[64], 0);
        assert_eq!(f64_at(72), -2.0);
        assert_eq!(f64_at(80), 7.0);
        assert_eq!(f64_at(88), 8.0);
        assert_eq!(f64_at(96), 10.0);
    }
}
