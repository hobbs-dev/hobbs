use std::env;
#[cfg(unix)]
use std::ffi::CStr;
use std::ffi::CString;
#[cfg(windows)]
use std::ffi::OsStr;
use std::fs::File;
use std::io::{BufWriter, Write};
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
type ContinuousProductionSweepFn = unsafe extern "C" fn(
    *mut f64,
    *const f64,
    *const f64,
    *const f64,
    *mut u64,
    *mut u64,
    *mut c_int,
) -> f64;
type ScalarCandidateFn = unsafe extern "C" fn(*mut f64, c_int, c_int, f64, f64) -> f64;
type ScalarProbeFn = unsafe extern "C" fn(*mut f64, c_int, c_int, f64, f64) -> f64;
type ScalarSliceTryFn = unsafe extern "C" fn(
    *mut f64,
    c_int,
    c_int,
    f64,
    f64,
    f64,
    *mut c_int,
) -> f64;
type ScalarAcceptFn = unsafe extern "C" fn();
type ScalarRejectFn = unsafe extern "C" fn(*mut f64, c_int, c_int, f64, f64);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SamplerKind {
    Rwmh,
    Slice,
    Gibbs,
}

impl SamplerKind {
    fn as_str(&self) -> &'static str {
        match self {
            SamplerKind::Rwmh => "rwmh",
            SamplerKind::Slice => "slice",
            SamplerKind::Gibbs => "gibbs",
        }
    }
}

#[derive(Debug, Clone)]
struct BlockSpec {
    name: String,
    offset: usize,
    len: usize,
    value_kind: ValueKind,
    sampler: SamplerKind,
    lower: i64,
    upper: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SaveRange {
    offset: usize,
    len: usize,
}

struct RuntimeBlock {
    name: String,
    offset: usize,
    len: usize,
    value_kind: ValueKind,
    sampler: SamplerKind,
    lower: i64,
    upper: i64,
    discrete_state_count: usize,
    f: BlockFn,
    continuous_adaptive_sweep: Option<ContinuousAdaptiveSweepFn>,
    continuous_production_sweep: Option<ContinuousProductionSweepFn>,
    continuous_sweep: Option<ContinuousSweepFn>,
    scalar_candidate: Option<ScalarCandidateFn>,
    scalar_probe: Option<ScalarProbeFn>,
    scalar_slice_try: Option<ScalarSliceTryFn>,
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
            let production_sweep_sym = format!("hobbs_sweep_sample_{}", b.name);
            let continuous_production_sweep = unsafe {
                optional_symbol(self.handle, &production_sweep_sym)
            }
            .map(|p| unsafe { std::mem::transmute::<*mut c_void, ContinuousProductionSweepFn>(p) });
            let sweep_sym = format!("hobbs_sweep_{}", b.name);
            let continuous_sweep = unsafe { optional_symbol(self.handle, &sweep_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ContinuousSweepFn>(p) });
            let candidate_sym = format!("hobbs_scalar_candidate_{}", b.name);
            let scalar_candidate = unsafe { optional_symbol(self.handle, &candidate_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ScalarCandidateFn>(p) });
            let probe_sym = format!("hobbs_scalar_probe_{}", b.name);
            let scalar_probe = unsafe { optional_symbol(self.handle, &probe_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ScalarProbeFn>(p) });
            let slice_try_sym = format!("hobbs_scalar_slice_try_{}", b.name);
            let scalar_slice_try = unsafe { optional_symbol(self.handle, &slice_try_sym) }
                .map(|p| unsafe { std::mem::transmute::<*mut c_void, ScalarSliceTryFn>(p) });
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
            let discrete_state_count = if b.value_kind == ValueKind::Discrete {
                discrete_state_count(b.lower, b.upper, &b.name)
            } else {
                0
            };
            out.push(RuntimeBlock {
                name: b.name.clone(),
                offset: b.offset,
                len: b.len,
                value_kind: b.value_kind,
                sampler: b.sampler,
                lower: b.lower,
                upper: b.upper,
                discrete_state_count,
                f,
                continuous_adaptive_sweep,
                continuous_production_sweep,
                continuous_sweep,
                scalar_candidate,
                scalar_probe,
                scalar_slice_try,
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
    adapt_diagnostics_out: Option<String>,
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
            adapt_diagnostics_out: None,
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
                // Accepted only for backward CLI compatibility. Scalar
                // Robbins-Monro adaptation updates every warmup sweep.
                let _ = parse_positive(val, "--adapt-every");
            }
            "--adapt-until" => cfg.adapt_until = Some(parse_usize(val, "--adapt-until")),
            "--target-accept" => {
                cfg.target_accept = parse_f64(val, "--target-accept");
                cfg.target_accept_set = true;
            }
            "--adapt-diagnostics-out" => cfg.adapt_diagnostics_out = Some(val.clone()),
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
    if cfg.adapt_until.is_none() {
        cfg.adapt_until = Some(cfg.burnin);
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
    if parts.len() != 3 && parts.len() != 4 && parts.len() != 5 && parts.len() != 7 {
        eprintln!("--block must have form name:offset:len[:indexed[:rwmh|slice]] or name:offset:len:indexed:discrete:lower:upper");
        process::exit(2);
    }
    let name = parts[0].to_string();
    let offset = parse_usize(parts[1], "--block offset");
    let len = parse_positive(parts[2], "--block len");
    if parts.len() >= 4 {
        match parts[3] {
            "indexed" | "scalar" | "element" => {}
            "group" | "joint" => {
                eprintln!("joint block proposals are not supported; use block name(j) for scalar parameter-local updates");
                process::exit(2);
            }
            _ => {
                eprintln!("--block kind must be indexed");
                process::exit(2);
            }
        }
    }

    let (value_kind, sampler, lower, upper) = if parts.len() == 7 {
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
        let sampler = if vk == ValueKind::Discrete {
            SamplerKind::Gibbs
        } else {
            SamplerKind::Rwmh
        };
        (vk, sampler, lo, hi)
    } else if parts.len() == 5 {
        let sampler = match parts[4] {
            "rwmh" | "mh" | "metropolis" => SamplerKind::Rwmh,
            "slice" => SamplerKind::Slice,
            _ => {
                eprintln!("--block continuous sampler must be rwmh or slice");
                process::exit(2);
            }
        };
        (ValueKind::Continuous, sampler, 0, 0)
    } else {
        (ValueKind::Continuous, SamplerKind::Rwmh, 0, 0)
    };

    BlockSpec {
        name,
        offset,
        len,
        value_kind,
        sampler,
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
        r#"hobbs: single-chain scalar parameter-local MCMC

Required:
  --lib PATH              Shared library exporting posterior_logp and/or posterior_logp_batch
  --data PATH             Optional data file passed to posterior_init(path)

Defaults:
  --dim 2
  --samples 1000          Saved samples after burn-in
  --burnin 500            Warmup/burn-in sweeps (`--warmups` is an alias)
  --thin 1
  --step 0.25             Initial RWMH proposal step size
  --adapt-every 25        Retained for compatibility; scalar adaptation runs every sweep
  --adapt-until burnin    Last iteration allowed to adapt; default equals --burnin
  --target-accept 0.44    Per-coordinate RWMH Robbins-Monro acceptance target
  --seed 1311768467463790320
  --out chain.bin         Binary output is the default
  --format bin            bin/binary or csv for chain output
  --save chain            chain or mean. mean writes only post-burn-in means
  --mean-range off:len    Parameter range averaged instead of written per draw; repeatable
  --mean-out PATH         One-row standard binary for declaration-level means
  --update global         full-target scalar RWMH or indexed parameter-local updates
  --block name:off:len:indexed:rwmh|slice      Continuous block sampler metadata
  --block name:off:len:indexed:discrete:lo:hi Exact finite-state Gibbs metadata
  --adapt-diagnostics-out PATH  Per-coordinate adaptation diagnostics CSV
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
    fn uniform_from_u64(raw: u64) -> f64 {
        let x = raw >> 11;
        ((x as f64) + 0.5) * (1.0 / ((1u64 << 53) as f64))
    }

    #[inline(always)]
    fn uniform_open01(&mut self) -> f64 {
        Self::uniform_from_u64(self.next_u64())
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
        "position,value_kind,proposals,accepted,acceptance_rate,warmup_proposals,warmup_accepted,warmup_acceptance_rate,proposal_sd,tuning_factor"
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
        writeln!(
            writer,
            "{},{},{},{},{},{},{},{},{},{}",
            position + 1,
            value_kinds[position].as_str(),
            proposals,
            accepted,
            format_optional_f64(acceptance_rate),
            warmup_proposals,
            warmup_accepted,
            format_optional_f64(warmup_acceptance_rate),
            format_optional_f64(is_continuous.then_some(proposal_scales[position])),
            format_optional_f64(is_continuous.then_some(tuning_factors[position]))
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
    min_scale: f64,
    max_scale: f64,
) -> f64 {
    (initial_step * tuning_factor).clamp(min_scale, max_scale)
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
        push_f64_slice(&mut self.buf, theta);
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
            push_f64_slice(
                &mut self.buf,
                &theta[range.offset..(range.offset + range.len)],
            );
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

#[inline(always)]
fn push_f64_slice(buf: &mut Vec<u8>, values: &[f64]) {
    #[cfg(target_endian = "little")]
    unsafe {
        let bytes = std::slice::from_raw_parts(
            values.as_ptr() as *const u8,
            std::mem::size_of_val(values),
        );
        buf.extend_from_slice(bytes);
    }

    #[cfg(target_endian = "big")]
    for &value in values {
        push_f64(buf, value);
    }
}

#[inline]
fn record_size(dim: usize) -> usize {
    8 + 8 + 8 + dim * 8 // iter + accepted/padding + logp + theta
}

struct ProgressDisplay {
    total: usize,
    last_percent: usize,
    next_iter: usize,
}

impl ProgressDisplay {
    fn new(total: usize) -> Self {
        Self {
            total,
            last_percent: usize::MAX,
            next_iter: 0,
        }
    }

    #[inline(always)]
    fn update(&mut self, iter: usize) {
        if self.total == 0 || iter < self.next_iter {
            return;
        }

        let percent = (((iter as u128) * 100u128) / (self.total as u128)).min(100) as usize;
        if percent == self.last_percent {
            return;
        }
        self.last_percent = percent;

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

        self.next_iter = if percent >= 100 {
            usize::MAX
        } else {
            let numerator = ((percent + 1) as u128) * (self.total as u128);
            ((numerator + 99) / 100).min(usize::MAX as u128) as usize
        };
    }
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
fn metropolis_accept_from_bits(log_alpha: f64, uniform_bits: u64) -> bool {
    if log_alpha.is_nan() || log_alpha == f64::NEG_INFINITY {
        false
    } else if log_alpha >= 0.0 {
        true
    } else {
        Xoshiro256StarStar::uniform_from_u64(uniform_bits) < log_alpha.exp()
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

const SLICE_STEPOUT_LIMIT: usize = 100;
const SLICE_SHRINK_LIMIT: usize = 10_000;
const SLICE_INITIAL_WIDTH: f64 = 1.0;
const SLICE_MIN_WIDTH: f64 = 1.0e-12;
const SLICE_MAX_WIDTH: f64 = 1.0e12;

#[derive(Debug, Clone, Copy)]
struct SliceUpdate {
    new_local: f64,
    new_value: f64,
    evaluations: u64,
    step_outs: u64,
    shrink_rejections: u64,
}

#[inline(always)]
fn has_generated_scalar_transaction(block: &RuntimeBlock) -> bool {
    block.scalar_candidate.is_some()
        && block.scalar_accept.is_some()
        && block.scalar_reject.is_some()
}

#[inline(always)]
fn begin_scalar_candidate(
    posterior: &PosteriorLib,
    block: &RuntimeBlock,
    theta: &mut [f64],
    index: c_int,
    position: usize,
    current_value: f64,
    proposed_value: f64,
) -> f64 {
    if has_generated_scalar_transaction(block) {
        return unsafe {
            block.scalar_candidate.unwrap_unchecked()(
                theta.as_mut_ptr(),
                index,
                position as c_int,
                current_value,
                proposed_value,
            )
        };
    }

    theta[position] = proposed_value;
    if let Some(update_fn) = block.cache_update {
        if block.cache_undo.is_none() && !block.cache_update_reversible {
            posterior.cache_snapshot();
        }
        unsafe {
            update_fn(theta.as_ptr(), index, current_value);
        }
    }
    unsafe { (block.f)(theta.as_ptr(), index) }
}

#[inline(always)]
fn probe_scalar_candidate(
    posterior: &PosteriorLib,
    block: &RuntimeBlock,
    theta: &mut [f64],
    index: c_int,
    position: usize,
    current_value: f64,
    proposed_value: f64,
) -> f64 {
    if let Some(probe) = block.scalar_probe {
        return unsafe {
            probe(
                theta.as_mut_ptr(),
                index,
                position as c_int,
                current_value,
                proposed_value,
            )
        };
    }

    let candidate = begin_scalar_candidate(
        posterior,
        block,
        theta,
        index,
        position,
        current_value,
        proposed_value,
    );
    reject_scalar_candidate(
        posterior,
        block,
        theta,
        index,
        position,
        current_value,
        proposed_value,
    );
    candidate
}

#[inline(always)]
fn reject_scalar_candidate(
    posterior: &PosteriorLib,
    block: &RuntimeBlock,
    theta: &mut [f64],
    index: c_int,
    position: usize,
    current_value: f64,
    proposed_value: f64,
) {
    if has_generated_scalar_transaction(block) {
        unsafe {
            block.scalar_reject.unwrap_unchecked()(
                theta.as_mut_ptr(),
                index,
                position as c_int,
                current_value,
                proposed_value,
            );
        }
        return;
    }

    if block.cache_update.is_some() {
        if let Some(undo_fn) = block.cache_undo {
            unsafe {
                undo_fn(theta.as_ptr(), index, current_value);
            }
            theta[position] = current_value;
        } else if block.cache_update_reversible {
            theta[position] = current_value;
            unsafe {
                block.cache_update.unwrap_unchecked()(theta.as_ptr(), index, proposed_value);
            }
        } else {
            posterior.cache_restore();
            theta[position] = current_value;
        }
    } else {
        theta[position] = current_value;
    }
}

#[inline(always)]
fn accept_scalar_candidate(block: &RuntimeBlock) {
    if has_generated_scalar_transaction(block) {
        unsafe {
            block.scalar_accept.unwrap_unchecked()();
        }
    }
}

#[inline(always)]
fn try_slice_candidate(
    posterior: &PosteriorLib,
    block: &RuntimeBlock,
    theta: &mut [f64],
    index: c_int,
    position: usize,
    current_value: f64,
    proposed_value: f64,
    log_slice: f64,
) -> (f64, bool) {
    if let Some(slice_try) = block.scalar_slice_try {
        let mut accepted: c_int = 0;
        let proposed_local = unsafe {
            slice_try(
                theta.as_mut_ptr(),
                index,
                position as c_int,
                current_value,
                proposed_value,
                log_slice,
                &mut accepted,
            )
        };
        return (proposed_local, accepted != 0);
    }

    let proposed_local = begin_scalar_candidate(
        posterior,
        block,
        theta,
        index,
        position,
        current_value,
        proposed_value,
    );
    let accepted = proposed_local.is_finite() && proposed_local >= log_slice;
    if accepted {
        accept_scalar_candidate(block);
    } else {
        reject_scalar_candidate(
            posterior,
            block,
            theta,
            index,
            position,
            current_value,
            proposed_value,
        );
    }
    (proposed_local, accepted)
}

#[inline(always)]
fn adapt_slice_width(width: &mut f64, tuning_factor: &mut f64, jump: f64, gain: f64) {
    if jump.is_finite() && jump > 0.0 && gain > 0.0 {
        // A width around twice the observed scalar jump gives the stepping-out
        // procedure enough room to find the slice quickly without creating an
        // unnecessarily wide shrinkage interval. The gain is computed once
        // per warmup sweep, not once per coordinate.
        let target_width = (2.0 * jump).clamp(SLICE_MIN_WIDTH, SLICE_MAX_WIDTH);
        *width = (*width + gain * (target_width - *width))
            .clamp(SLICE_MIN_WIDTH, SLICE_MAX_WIDTH);
    }
    *tuning_factor = (*width / SLICE_INITIAL_WIDTH).clamp(SLICE_MIN_WIDTH, SLICE_MAX_WIDTH);
}

#[inline]
fn slice_update_coordinate(
    posterior: &PosteriorLib,
    block: &RuntimeBlock,
    theta: &mut [f64],
    index: c_int,
    position: usize,
    current_local: f64,
    width: f64,
    rng: &mut Xoshiro256StarStar,
) -> SliceUpdate {
    let current_value = theta[position];
    let width = width
        .max(8.0 * f64::EPSILON * (1.0 + current_value.abs()))
        .clamp(SLICE_MIN_WIDTH, SLICE_MAX_WIDTH);
    let log_slice = current_local + rng.uniform_open01().ln();

    // Randomly position the initial interval around the current point, then
    // use Neal's m-limited stepping-out construction. Randomly splitting the
    // finite step-out budget between left and right preserves reversibility.
    let mut left = current_value - width * rng.uniform_open01();
    let mut right = left + width;
    let mut left_steps = (rng.uniform_open01() * SLICE_STEPOUT_LIMIT as f64) as usize;
    if left_steps >= SLICE_STEPOUT_LIMIT {
        left_steps = SLICE_STEPOUT_LIMIT - 1;
    }
    let mut right_steps = SLICE_STEPOUT_LIMIT - 1 - left_steps;

    let mut evaluations = 0u64;
    let mut step_outs = 0u64;
    let mut shrink_rejections = 0u64;

    while left_steps > 0 {
        let candidate_local = probe_scalar_candidate(
            posterior,
            block,
            theta,
            index,
            position,
            current_value,
            left,
        );
        evaluations += 1;
        if !candidate_local.is_finite() || candidate_local <= log_slice {
            break;
        }
        left -= width;
        left_steps -= 1;
        step_outs += 1;
    }

    while right_steps > 0 {
        let candidate_local = probe_scalar_candidate(
            posterior,
            block,
            theta,
            index,
            position,
            current_value,
            right,
        );
        evaluations += 1;
        if !candidate_local.is_finite() || candidate_local <= log_slice {
            break;
        }
        right += width;
        right_steps -= 1;
        step_outs += 1;
    }

    for _ in 0..SLICE_SHRINK_LIMIT {
        let proposed_value = left + (right - left) * rng.uniform_open01();
        let (proposed_local, accepted) = try_slice_candidate(
            posterior,
            block,
            theta,
            index,
            position,
            current_value,
            proposed_value,
            log_slice,
        );
        evaluations += 1;

        if accepted {
            return SliceUpdate {
                new_local: proposed_local,
                new_value: proposed_value,
                evaluations,
                step_outs,
                shrink_rejections,
            };
        }

        shrink_rejections += 1;
        if proposed_value < current_value {
            left = proposed_value;
        } else {
            right = proposed_value;
        }
    }

    eprintln!(
        "slice sampler exceeded {} shrinkage evaluations for block {}({})",
        SLICE_SHRINK_LIMIT, block.name, index
    );
    process::exit(1);
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
    for block in &blocks {
        let end = block.offset.checked_add(block.len).unwrap_or_else(|| {
            eprintln!("block {} offset/length overflow", block.name);
            process::exit(2);
        });
        if end > dim {
            eprintln!("block {} offset/length exceeds dim", block.name);
            process::exit(2);
        }
        for position in block.offset..end {
            if covered[position] {
                eprintln!(
                    "parameter position {} is covered by more than one scalar block",
                    position + 1
                );
                process::exit(2);
            }
            covered[position] = true;
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
        for position in block.offset..(block.offset + block.len) {
            value_kinds[position] = block.value_kind;
        }
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

    let mut proposal_scales = vec![cfg.step; dim];
    let mut tuning_factors = vec![1.0; dim];
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

    let continuous_positions: Vec<usize> = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous)
        .flat_map(|block| block.offset..(block.offset + block.len))
        .collect();
    let rwmh_positions: Vec<usize> = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous && block.sampler == SamplerKind::Rwmh)
        .flat_map(|block| block.offset..(block.offset + block.len))
        .collect();
    let slice_positions: Vec<usize> = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous && block.sampler == SamplerKind::Slice)
        .flat_map(|block| block.offset..(block.offset + block.len))
        .collect();
    for &position in &continuous_positions {
        coordinate_adapt_proposals[position] = adapt_until as u64;
    }
    for &position in &slice_positions {
        proposal_scales[position] = SLICE_INITIAL_WIDTH;
        tuning_factors[position] = 1.0;
    }

    let max_continuous_len = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous && block.sampler == SamplerKind::Rwmh)
        .map(|block| block.len)
        .max()
        .unwrap_or(0);
    let max_discrete_values = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Discrete)
        .map(|block| block.discrete_state_count)
        .max()
        .unwrap_or(0);
    let mut normals = vec![0.0f64; max_continuous_len];
    let mut uniforms = vec![0.0f64; max_continuous_len];
    let mut accepted_flags = vec![0u8; max_continuous_len];
    let mut discrete_logps = vec![f64::NEG_INFINITY; max_discrete_values];
    let mut discrete_weights = vec![0.0f64; max_discrete_values];

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
                && block.sampler == SamplerKind::Rwmh
                && (block.continuous_adaptive_sweep.is_some()
                    || block.continuous_production_sweep.is_some()
                    || block.continuous_sweep.is_some())
        })
        .count();
    let production_sweep_count = blocks
        .iter()
        .filter(|block| {
            block.value_kind == ValueKind::Continuous
                && block.sampler == SamplerKind::Rwmh
                && block.continuous_production_sweep.is_some()
        })
        .count();
    let fused_sweep_count = blocks
        .iter()
        .filter(|block| {
            block.value_kind == ValueKind::Continuous
                && block.sampler == SamplerKind::Rwmh
                && block.len >= FUSED_ADAPTIVE_SWEEP_MIN_LEN
                && block.continuous_adaptive_sweep.is_some()
        })
        .count();
    let rwmh_block_count = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous && block.sampler == SamplerKind::Rwmh)
        .count();
    let slice_block_count = blocks
        .iter()
        .filter(|block| block.value_kind == ValueKind::Continuous && block.sampler == SamplerKind::Slice)
        .count();

    if !cfg.quiet {
        eprintln!("  update mode:          scalar parameter-local samplers");
        eprintln!(
            "  block functions:      {}",
            blocks
                .iter()
                .map(|block| {
                    if block.value_kind == ValueKind::Discrete {
                        format!(
                            "{}:scalar:{}:{}[{},{}]",
                            block.name,
                            block.value_kind.as_str(),
                            block.sampler.as_str(),
                            block.lower,
                            block.upper
                        )
                    } else {
                        format!(
                            "{}:scalar:{}:{}",
                            block.name,
                            block.value_kind.as_str(),
                            block.sampler.as_str()
                        )
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        );
        eprintln!("  scalar parameters:    {}", dim);
        eprintln!("  rwmh blocks:          {}", rwmh_block_count);
        eprintln!("  slice blocks:         {}", slice_block_count);
        eprintln!(
            "  generated C sweeps:   {}/{} rwmh blocks",
            generated_sweep_count, rwmh_block_count
        );
        eprintln!(
            "  fused adapt sweeps:   {}/{} rwmh blocks",
            fused_sweep_count, rwmh_block_count
        );
        eprintln!(
            "  frozen C sweeps:      {}/{} rwmh blocks",
            production_sweep_count, rwmh_block_count
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
    }

    let start = std::time::Instant::now();
    let mut slice_local_evaluations = 0u64;
    let mut slice_step_outs = 0u64;
    let mut slice_shrink_rejections = 0u64;
    let mut progress = ProgressDisplay::new(total_iters);
    if !cfg.quiet {
        progress.update(0);
    }

    for iter in 1..=total_iters {
        let mut accepted_sweep = false;
        let adapting = iter <= adapt_until;
        let (scale_up, scale_down) = if adapting {
            adaptation_multipliers(iter, cfg.target_accept)
        } else {
            (1.0, 1.0)
        };
        let slice_adapt_gain = if adapting && !slice_positions.is_empty() {
            ((iter as f64) + 10.0).powf(-0.6)
        } else {
            0.0
        };

        for block in &blocks {
            if block.value_kind == ValueKind::Continuous {
                if block.sampler == SamplerKind::Slice {
                    for local in 0..block.len {
                        let index = (local + 1) as c_int;
                        let position = block.offset + local;
                        let old_value = theta[position];
                        let old_local = unsafe { (block.f)(theta.as_ptr(), index) };
                        if !old_local.is_finite() {
                            eprintln!(
                                "slice block {}({}) gave non-finite current local logp: {}",
                                block.name, index, old_local
                            );
                            process::exit(1);
                        }

                        let update = slice_update_coordinate(
                            posterior,
                            block,
                            &mut theta,
                            index,
                            position,
                            old_local,
                            proposal_scales[position],
                            &mut rng,
                        );
                        current_logp += update.new_local - old_local;
                        coordinate_accepts[position] += 1;
                        accepted_sweep = true;
                        slice_local_evaluations = slice_local_evaluations.saturating_add(update.evaluations);
                        slice_step_outs = slice_step_outs.saturating_add(update.step_outs);
                        slice_shrink_rejections = slice_shrink_rejections.saturating_add(update.shrink_rejections);

                        if adapting {
                            coordinate_adapt_accepts[position] += 1;
                            adapt_slice_width(
                                &mut proposal_scales[position],
                                &mut tuning_factors[position],
                                (update.new_value - old_value).abs(),
                                slice_adapt_gain,
                            );
                        }
                    }
                    continue;
                }

                if !adapting {
                    if let Some(sweep_fn) = block.continuous_production_sweep {
                        for local in 0..block.len {
                            normals[local] = rng.normal();
                            uniforms[local] = rng.uniform_open01();
                        }
                        let mut bad_index: c_int = 0;
                        let mut accepted_in_block = 0u64;
                        let scale_slice =
                            &proposal_scales[block.offset..(block.offset + block.len)];
                        let delta_sum = unsafe {
                            sweep_fn(
                                theta.as_mut_ptr(),
                                scale_slice.as_ptr(),
                                normals.as_ptr(),
                                uniforms.as_ptr(),
                                coordinate_accepts.as_mut_ptr().add(block.offset),
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
                        if accepted_in_block > 0 {
                            accepted_sweep = true;
                        }
                        continue;
                    }
                }

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
                        if accepted {
                            coordinate_accepts[position] += 1;
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
                    let accepted = if new_local.is_finite() {
                        let uniform_bits = rng.next_u64();
                        metropolis_accept_from_bits(log_alpha, uniform_bits)
                    } else {
                        false
                    };
                    if accepted {
                        current_logp += log_alpha;
                        coordinate_accepts[position] += 1;
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
                let state_count = block.discrete_state_count;
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
                for state_index in 0..state_count {
                    let value_logp = discrete_logps[state_index];
                    let weight = if value_logp.is_finite() {
                        (value_logp - max_logp).exp()
                    } else {
                        0.0
                    };
                    discrete_weights[state_index] = weight;
                    total_weight += weight;
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
                for (state_index, &weight) in discrete_weights[..state_count].iter().enumerate() {
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
                    coordinate_accepts[position] += 1;
                    accepted_sweep = true;
                } else {
                    theta[position] = old_value;
                }

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


        if iter > cfg.burnin && ((iter - cfg.burnin) % cfg.thin == 0) {
            retained_output.retain(iter as u64, current_logp, accepted_sweep, &theta);
        }
        if !cfg.quiet {
            progress.update(iter);
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
        );
    }

    if !cfg.quiet {
        let seconds = start.elapsed().as_secs_f64();
        let total_updates = (total_iters as u64).saturating_mul(dim as u64);
        let rwmh_proposals =
            (total_iters as u64).saturating_mul(rwmh_positions.len() as u64);
        let rwmh_accepts: u64 = rwmh_positions
            .iter()
            .map(|&position| coordinate_accepts[position])
            .sum();
        let slice_updates =
            (total_iters as u64).saturating_mul(slice_positions.len() as u64);
        let discrete_position_count = dim - continuous_positions.len();
        let discrete_updates =
            (total_iters as u64).saturating_mul(discrete_position_count as u64);
        let discrete_moves: u64 = value_kinds
            .iter()
            .zip(coordinate_accepts.iter())
            .filter_map(|(kind, &accepted)| {
                (*kind == ValueKind::Discrete).then_some(accepted)
            })
            .sum();
        eprintln!("done");
        eprintln!("  dim:                  {}", dim);
        eprintln!("  sweeps:               {}", total_iters);
        eprintln!("  scalar updates:       {}", total_updates);
        eprintln!("  saved samples:        {}", cfg.samples);

        if !rwmh_positions.is_empty() {
            let mut scale_min = f64::INFINITY;
            let mut scale_max = f64::NEG_INFINITY;
            let mut scale_sum = 0.0;
            let mut rate_min = f64::INFINITY;
            let mut rate_max = f64::NEG_INFINITY;
            let mut rate_sum = 0.0;
            for &position in &rwmh_positions {
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
            let count = rwmh_positions.len() as f64;
            eprintln!(
                "  rwmh accept:          {:.4}",
                rwmh_accepts as f64 / rwmh_proposals.max(1) as f64
            );
            eprintln!(
                "  rwmh coord rate:      mean {:.4}, min {:.4}, max {:.4}",
                rate_sum / count,
                rate_min,
                rate_max
            );
            eprintln!(
                "  rwmh proposal sd:     mean {:.6e}, min {:.6e}, max {:.6e}",
                scale_sum / count,
                scale_min,
                scale_max
            );
            eprintln!("  rwmh target accept:   {:.4}", cfg.target_accept);
        }
        if !slice_positions.is_empty() {
            let mut width_min = f64::INFINITY;
            let mut width_max = f64::NEG_INFINITY;
            let mut width_sum = 0.0;
            for &position in &slice_positions {
                let width = proposal_scales[position];
                width_min = width_min.min(width);
                width_max = width_max.max(width);
                width_sum += width;
            }
            let count = slice_positions.len() as f64;
            let updates = slice_updates.max(1) as f64;
            eprintln!(
                "  slice width:          mean {:.6e}, min {:.6e}, max {:.6e}",
                width_sum / count,
                width_min,
                width_max
            );
            eprintln!(
                "  slice local evals:    {:.3} per update",
                slice_local_evaluations as f64 / updates
            );
            eprintln!(
                "  slice step-outs:      {:.3} per update",
                slice_step_outs as f64 / updates
            );
            eprintln!(
                "  slice shrink rejects: {:.3} per update",
                slice_shrink_rejections as f64 / updates
            );
        }
        if discrete_updates > 0 {
            eprintln!(
                "  discrete move rate:   {:.4}",
                discrete_moves as f64 / discrete_updates as f64
            );
        }
        eprintln!("  adapt until:          {}", adapt_until);
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
    let min_tuning = (-20.0f64).exp();
    let max_tuning = 20.0f64.exp();
    let min_scale = (cfg.step * min_tuning).max(1e-12);
    let max_scale = (cfg.step * max_tuning).min(1e12);
    // Every coordinate is visited once per sweep, so proposal counts are
    // deterministic and do not need hot-loop increments.
    let proposals = vec![total_iters as u64; dim];
    let mut accepts = vec![0u64; dim];
    let adapt_proposals = vec![adapt_until as u64; dim];
    let mut adapt_accepts = vec![0u64; dim];

    if !cfg.quiet {
        eprintln!("  update mode:          full-target adaptive scalar Metropolis-within-Gibbs");
        eprintln!("  scalar parameters:    {}", dim);
    }

    let start = std::time::Instant::now();
    let mut progress = ProgressDisplay::new(total_iters);
    if !cfg.quiet {
        progress.update(0);
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
            let accepted = if proposed_logp.is_finite() {
                let uniform_bits = rng.next_u64();
                metropolis_accept_from_bits(log_alpha, uniform_bits)
            } else {
                false
            };
            if accepted {
                current_logp = proposed_logp;
                accepts[position] += 1;
                accepted_sweep = true;
            } else {
                theta[position] = old_value;
            }
            if adapting {
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
                    min_scale,
                    max_scale,
                );
            }
        }


        if iter > cfg.burnin && ((iter - cfg.burnin) % cfg.thin == 0) {
            retained_output.retain(iter as u64, current_logp, accepted_sweep, &theta);
        }
        if !cfg.quiet {
            progress.update(iter);
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
        );
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
        let total_proposals = (total_iters as u64).saturating_mul(dim as u64);
        let accepted_total: u64 = accepts.iter().copied().sum();
        eprintln!("done");
        eprintln!("  dim:                  {}", dim);
        eprintln!("  sweeps:               {}", total_iters);
        eprintln!("  scalar updates:       {}", total_proposals);
        eprintln!("  saved samples:        {}", cfg.samples);
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
    #[test]
    fn block_spec_accepts_slice_sampler() {
        let spec = parse_block_spec("u:4:12:indexed:slice");
        assert_eq!(spec.name, "u");
        assert_eq!(spec.offset, 4);
        assert_eq!(spec.len, 12);
        assert_eq!(spec.value_kind, ValueKind::Continuous);
        assert_eq!(spec.sampler, SamplerKind::Slice);
    }

    #[test]
    fn slice_sampler_has_standard_normal_moments() {
        unsafe extern "C" fn standard_normal_block(theta: *const f64, _index: c_int) -> f64 {
            let x = unsafe { *theta };
            -0.5 * x * x
        }

        let posterior = std::mem::ManuallyDrop::new(PosteriorLib {
            handle: std::ptr::null_mut(),
            logp: None,
            logp_batch: None,
            init: None,
            free: None,
            cache_init: None,
            cache_free: None,
            cache_snapshot: None,
            cache_restore: None,
            cache_initialized: false,
            initialized: false,
            mode: EvalMode::Scalar,
            one_out: [0.0; 1],
        });
        let block = RuntimeBlock {
            name: "x".to_string(),
            offset: 0,
            len: 1,
            value_kind: ValueKind::Continuous,
            sampler: SamplerKind::Slice,
            lower: 0,
            upper: 0,
            discrete_state_count: 0,
            f: standard_normal_block,
            continuous_adaptive_sweep: None,
            continuous_production_sweep: None,
            continuous_sweep: None,
            scalar_candidate: None,
            scalar_probe: None,
            scalar_slice_try: None,
            scalar_accept: None,
            scalar_reject: None,
            cache_update: None,
            cache_undo: None,
            cache_update_reversible: false,
        };
        let mut theta = [0.0f64];
        let mut rng = Xoshiro256StarStar::new(0x51_1ce_5eed);
        let mut width = SLICE_INITIAL_WIDTH;
        let mut tuning = 1.0;
        let warmup = 2_000usize;
        let samples = 100_000usize;
        let mut sum = 0.0;
        let mut sumsq = 0.0;

        for iter in 1..=(warmup + samples) {
            let old_value = theta[0];
            let old_local = -0.5 * old_value * old_value;
            let update = slice_update_coordinate(
                &posterior,
                &block,
                &mut theta,
                1,
                0,
                old_local,
                width,
                &mut rng,
            );
            if iter <= warmup {
                adapt_slice_width(
                    &mut width,
                    &mut tuning,
                    (update.new_value - old_value).abs(),
                    ((iter as f64) + 10.0).powf(-0.6),
                );
            } else {
                sum += theta[0];
                sumsq += theta[0] * theta[0];
            }
        }

        let mean = sum / samples as f64;
        let variance = sumsq / samples as f64 - mean * mean;
        assert!(mean.abs() < 0.025, "mean={mean}");
        assert!((variance - 1.0).abs() < 0.04, "variance={variance}");
        assert!(width.is_finite() && width > 0.0, "width={width}");
    }

}
