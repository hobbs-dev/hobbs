#' Build the bundled hobbs Rust sampler
#'
#' Builds the Rust command-line sampler bundled in this package. Usually this is
#' called automatically by `hobbs()` the first time it is needed.
#'
#' @param rebuild Logical. If TRUE, force a fresh cargo build.
#' @param quiet Logical. If TRUE, suppress build output.
#' @return Path to the hobbs executable.
#' @export
hobbs_build_sampler <- function(rebuild = FALSE, quiet = TRUE) {
  src <- system.file("hobbs", package = "hobbs", mustWork = TRUE)
  cache_root <- hobbs_cache_dir()
  dst <- file.path(cache_root, "hobbs")
  rust_target <- hobbs_rust_target()
  exe <- hobbs_sampler_executable(dst, rust_target)

  # The Rust sampler is cached outside the installed package. Invalidate that
  # cache when the R package version changes, otherwise an older cached binary
  # can silently lack newer CLI features such as --data.
  stamp <- file.path(dst, ".hobbs_package_version")
  target_stamp <- file.path(dst, ".hobbs_rust_target")
  pkg_version <- as.character(utils::packageVersion("hobbs"))
  target_id <- if (is.null(rust_target)) "host" else rust_target
  cached_version <- if (file.exists(stamp)) readLines(stamp, warn = FALSE, n = 1L) else NA_character_
  cached_target <- if (file.exists(target_stamp)) readLines(target_stamp, warn = FALSE, n = 1L) else NA_character_
  if (dir.exists(dst) && (
    !identical(cached_version, pkg_version) ||
    (!is.null(rust_target) && !identical(cached_target, target_id))
  )) {
    unlink(dst, recursive = TRUE, force = TRUE)
  }

  if (rebuild && dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
  if (!dir.exists(dst)) {
    dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
    copy_dir(src, dst)
    writeLines(pkg_version, stamp)
    writeLines(target_id, target_stamp)
  }

  if (!file.exists(exe) || rebuild) {
    cargo <- Sys.which("cargo")
    if (!nzchar(cargo)) stop("Could not find `cargo` on PATH. Install Rust from https://rustup.rs/ and try again.", call. = FALSE)
    ensure_rust_target(rust_target, quiet = quiet)
    out <- if (quiet) FALSE else ""
    err <- if (quiet) FALSE else ""
    cargo_args <- c("build", "--release", "--locked")
    if (!is.null(rust_target)) cargo_args <- c(cargo_args, "--target", rust_target)
    status <- run_in_dir(dst, cargo, cargo_args, stdout = out, stderr = err)
    if (!identical(status, 0L)) stop("cargo build --release --locked failed", call. = FALSE)
  }

  normalizePath(exe, mustWork = TRUE)
}

#' Check the hobbs compilation toolchain
#'
#' Checks for Cargo and Rust, which are required to build the hobbs sampler,
#' and a C compiler, which is required to compile hobbs models.
#'
#' @param quiet Logical. If `TRUE`, suppress status messages.
#' @param stop_on_error Logical. If `TRUE`, stop when any required tool is
#'   unavailable.
#'
#' @return Invisibly returns a named character vector containing the paths to
#'   `cargo`, `rustc`, and the detected C compiler.
#' @export
hobbs_check_toolchain <- function(quiet = FALSE, stop_on_error = FALSE) {
    if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
        stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
    }
    
    if (
        !is.logical(stop_on_error) ||
        length(stop_on_error) != 1L ||
        is.na(stop_on_error)
    ) {
        stop("`stop_on_error` must be TRUE or FALSE.", call. = FALSE)
    }
    
    find_first <- function(commands) {
        paths <- Sys.which(commands)
        paths <- unname(paths[nzchar(paths)])
        
        if (length(paths)) {
            normalizePath(paths[[1L]], mustWork = FALSE)
        } else {
            ""
        }
    }
    
    cargo <- find_first("cargo")
    rustc <- find_first("rustc")
    
    c_candidates <- if (.Platform$OS.type == "windows") {
        c("gcc", "clang", "cc")
    } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
        c("clang", "cc", "gcc")
    } else {
        c("cc", "gcc", "clang")
    }
    
    tools <- c(
        cargo = cargo,
        rustc = rustc,
        c_compiler = find_first(c_candidates)
    )
    
    available <- stats::setNames(
        nzchar(unname(tools)),
        names(tools)
    )
    
    labels <- c(
        cargo = "Cargo",
        rustc = "Rust compiler",
        c_compiler = "C compiler"
    )
    
    if (!quiet) {
        for (name in names(tools)) {
            if (isTRUE(available[[name]])) {
                message(
                    sprintf(
                        "%-14s [OK] %s",
                        labels[[name]],
                        tools[[name]]
                    )
                )
            } else {
                message(
                    sprintf(
                        "%-14s [NOT FOUND]",
                        labels[[name]]
                    )
                )
            }
        }
    }
    
    if (!all(available)) {
        missing <- names(available)[!available]
        instructions <- character()
        
        if (any(c("cargo", "rustc") %in% missing)) {
            instructions <- c(
                instructions,
                "Install Rust and Cargo from https://rustup.rs/."
            )
        }
        
        if ("c_compiler" %in% missing) {
            c_instruction <- if (.Platform$OS.type == "windows") {
                paste0(
                    "Install the version of Rtools appropriate for your version of R ",
                    "from https://cran.r-project.org/bin/windows/Rtools/."
                )
            } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
                paste0(
                    "Install the Xcode Command Line Tools by running ",
                    "`xcode-select --install` in Terminal."
                )
            } else {
                paste0(
                    "Install a C compiler, such as GCC or Clang, using your system ",
                    "package manager."
                )
            }
            
            instructions <- c(instructions, c_instruction)
        }
        
        msg <- paste(
            c(
                paste0(
                    "hobbs could not find: ",
                    paste(labels[missing], collapse = ", "),
                    "."
                ),
                instructions
            ),
            collapse = "\n"
        )
        
        if (stop_on_error) {
            stop(msg, call. = FALSE)
        }
        
        if (!quiet) {
            warning(msg, call. = FALSE)
        }
    }
    
    invisible(tools)
}


#' Install the hobbs sampler
#'
#' Compiles and installs the bundled Rust sampler in the hobbs user cache.
#' The compiled sampler is reused by subsequent hobbs sessions and models.
#'
#' @param rebuild Logical. If `TRUE`, remove and rebuild the cached sampler.
#' @param quiet Logical. If `TRUE`, suppress build output and status messages.
#'
#' @return Invisibly returns the path to the installed hobbs sampler.
#' @export
hobbs_install_sampler <- function(rebuild = FALSE, quiet = FALSE) {
    if (!is.logical(rebuild) || length(rebuild) != 1L || is.na(rebuild)) {
        stop("`rebuild` must be TRUE or FALSE.", call. = FALSE)
    }
    
    if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
        stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
    }
    
    hobbs_check_toolchain(
        quiet = quiet,
        stop_on_error = TRUE
    )
    
    if (!quiet) {
        if (rebuild) {
            message("Rebuilding the hobbs sampler...")
        } else {
            message("Installing the hobbs sampler...")
        }
    }
    
    sampler <- hobbs_build_sampler(
        rebuild = rebuild,
        quiet = quiet
    )
    
    if (!file.exists(sampler)) {
        stop(
            "The sampler build completed without creating an executable.",
            call. = FALSE
        )
    }
    
    if (!quiet) {
        message("hobbs sampler installed successfully:")
        message(sampler)
    }
    
    invisible(sampler)
}


#' Check the installed hobbs sampler
#'
#' Checks whether a compatible hobbs sampler is installed in the user cache
#' and verifies that the executable can be started.
#'
#' @param quiet Logical. If `TRUE`, suppress status messages.
#'
#' @return Invisibly returns `TRUE` when a compatible sampler is installed and
#'   working, and `FALSE` otherwise.
#' @export
hobbs_check_sampler <- function(quiet = FALSE) {
    if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
        stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
    }
    
    sampler_dir <- file.path(hobbs_cache_dir(), "hobbs")
    
    sampler <- hobbs_sampler_executable(
        sampler_dir,
        hobbs_rust_target()
    )
    
    version_file <- file.path(
        sampler_dir,
        ".hobbs_package_version"
    )
    
    package_version <- as.character(
        utils::packageVersion("hobbs")
    )
    
    cached_version <- if (file.exists(version_file)) {
        trimws(readLines(version_file, warn = FALSE, n = 1L))
    } else {
        NA_character_
    }
    
    if (!file.exists(sampler)) {
        if (!quiet) {
            message(
                "hobbs sampler is not installed. Run ",
                "`hobbs_install_sampler()` to install it."
            )
        }
        
        return(invisible(FALSE))
    }
    
    if (
        is.na(cached_version) ||
        !identical(cached_version, package_version)
    ) {
        if (!quiet) {
            message(
                "The cached hobbs sampler was built for package version ",
                if (is.na(cached_version)) "unknown" else cached_version,
                ", but the installed package version is ",
                package_version,
                ". Run `hobbs_install_sampler(rebuild = TRUE)`."
            )
        }
        
        return(invisible(FALSE))
    }
    
    output <- tryCatch(
        suppressWarnings(
            system2(
                sampler,
                "--help",
                stdout = TRUE,
                stderr = TRUE
            )
        ),
        error = identity
    )
    
    if (inherits(output, "error")) {
        if (!quiet) {
            message(
                "The hobbs sampler exists but could not be started: ",
                conditionMessage(output),
                "\nRun `hobbs_install_sampler(rebuild = TRUE)`."
            )
        }
        
        return(invisible(FALSE))
    }
    
    status <- attr(output, "status")
    
    if (!is.null(status) && !identical(as.integer(status), 0L)) {
        if (!quiet) {
            message(
                "The hobbs sampler exists but failed its startup check with status ",
                status,
                ". Run `hobbs_install_sampler(rebuild = TRUE)`."
            )
        }
        
        return(invisible(FALSE))
    }
    
    if (!quiet) {
        message("hobbs sampler is installed and working:")
        message(normalizePath(sampler, mustWork = TRUE))
    }
    
    invisible(TRUE)
}

#' Compile and sample a hobbs Bayesian model
#'
#' @description
#' `hobbs()` is the main model-fitting interface for **hobbs** (High dimensiOnal
#' Bayesian omniBus Sampler), a probabilistic programming system designed for
#' high-dimensional Bayesian models with sparse or structured computational
#' dependencies. Models are written in a compact C-like language using
#' parameter declarations, reusable code chunks, probability statements, and
#' parameter-local sampling blocks.
#'
#' The R front end translates the model and R data into optimized C code and
#' compiles it as a shared library. A reusable Rust runtime then performs the
#' MCMC sweeps, proposal adaptation, output, and diagnostics. In block mode,
#' model-specific posterior calculations and deterministic cache updates remain
#' in generated C so that each scalar proposal can perform only the work that
#' its dependency structure requires.
#'
#' @details
#' The preferred interface declares model parameters directly in the model
#' source with `param` for continuous parameters and `dparam` for bounded
#' discrete parameters. Parameter and data accessors use one-based indexing,
#' matching R. For example, `param beta(p);` creates `beta(1)`, ..., `beta(p)`,
#' and a matrix supplied as `X` in `data` can be read as `X(i, j)`.
#'
#' Probability statements have the form
#' `value ~ distribution(arguments);` and add the corresponding log density or
#' log probability to the current block target. Repeated calculations can be
#' placed in `func name() { ... }` declarations. These are reusable code chunks
#' that are expanded at their call sites before C compilation, so they can use
#' indices and local declarations from the surrounding block. Ordinary C
#' expressions, scalar declarations, loops, conditionals, transformations, and
#' direct additions to `target` can also be used. Temporary `vec` and `mat`
#' declarations are available for vector and matrix calculations used by
#' multivariate distributions.
#'
#' @section Toolchain:
#' hobbs models are compiled at run time. A working C compiler and Rust with
#' Cargo are therefore required. Use [hobbs_check_toolchain()] to diagnose the
#' local toolchain, [hobbs_install_sampler()] to build the bundled Rust sampler
#' into the user cache, and [hobbs_check_sampler()] to verify that the cached
#' sampler starts correctly. `hobbs()` builds or reuses the bundled sampler
#' automatically when `binary = NULL`.
#'
#' @section Parameter-local blocks:
#' A declaration such as
#'
#' ```
#' block beta(j) {
#'   beta(j) ~ dnorm(0, 10);
#'   llk();
#' }
#' ```
#'
#' creates one scalar update for each coordinate of `beta`. The block need not
#' evaluate the complete log posterior. It should evaluate exactly the prior,
#' likelihood, and other posterior terms whose values can change when that
#' coordinate changes. Terms that do not depend on the proposed coordinate may
#' be omitted because they cancel from the Metropolis-Hastings ratio.
#'
#' This is an exact computation, not an approximation, provided that every
#' posterior contribution affected by the proposal is included. A `block` is
#' therefore both a sampling declaration and an explicit dependency contract.
#' This lets grouped, sparse, latent-variable, and variable-selection models
#' restrict work to the observations or terms actually affected by a proposal.
#'
#' Continuous coordinates use sequential scalar Gaussian random-walk
#' Metropolis updates. Bounded discrete coordinates declared with, for example,
#' `dparam z(n, 0, 1);` are updated by evaluating their local block target over
#' every value in the declared support and drawing from the resulting
#' finite-state conditional distribution.
#'
#' @section Persistent deterministic caches:
#' A block can maintain exact deterministic state with attached `cache` and
#' `update` declarations. For example, a cached linear predictor can be
#' initialized once and updated after a proposal to `beta(j)` using
#'
#' ```
#' mu(i) += (proposal(beta(j)) - current(beta(j))) * X(i, j);
#' ```
#'
#' `proposal(...)` is the proposed scalar value and `current(...)` is the
#' currently accepted value. Cache updates are transactional: the proposed
#' parameter and updated cache are used together to evaluate the block target;
#' on rejection, hobbs restores the previous cache automatically. A cache can
#' be maintained by multiple blocks. Correctness requires that its initializer
#' and every attached update keep the cached quantity algebraically consistent
#' with the current parameter state.
#'
#' Persistent deterministic caches are exact and are distinct from the optional
#' lookup-table approximation controlled by `log_cache`.
#'
#' @section Sampling and adaptation:
#' Each continuous coordinate is attempted once per sweep. During warmup,
#' hobbs adapts a coordinate-specific Gaussian random-walk proposal scale using
#' Robbins-Monro stochastic approximation. By default the target acceptance
#' probability is `0.44`, appropriate for the one-dimensional proposals used by
#' the sampler. Adaptation normally continues through the warmup period and the
#' resulting scales are then fixed for retained sampling. Discrete finite-state
#' Gibbs updates do not require proposal-scale adaptation.
#'
#' `warmups` is the preferred name for the discarded warmup length; `burnin` is
#' retained for compatibility. Supply only one of them. `adapt_until` can be
#' used to control the last adaptation iteration explicitly.
#'
#' @section Output and high-dimensional storage:
#' By default, retained draws are written to a binary chain and can be read with
#' [read_hobbs()]. A parameter declaration can include `save=mean`, for example
#' `param u(m, 2) save=mean;`. The parameter remains part of the Markov state and
#' is sampled normally, but only its post-warmup posterior mean is retained.
#' This separates the dimension of the sampled state from the dimension of the
#' stored chain and can greatly reduce storage for large nuisance parameter
#' arrays. Mean-only parameters do not retain information needed for posterior
#' quantiles or convergence diagnostics; use [read_hobbs_mean()] to read their
#' saved means when mixed with full-chain parameters.
#'
#' The returned `hobbs_run` object records the generated source and shared
#' library, output paths, model dimensions and parameter names, block metadata,
#' adaptation diagnostics, proposal settings, and process status. It can be
#' passed directly to [read_hobbs()].
#'
#' @section Built-in probability statements:
#' Sampling statements currently include scalar continuous distributions
#' `dnorm`, `normal01`, `normal_sd1`, `dunif`, `dexp`, `dgamma`, `dinvgamma`,
#' `dbeta`, `dcauchy`, `dt`, `dchisq`, `dlnorm`, `dlogis`, `dlaplace`,
#' `dweibull`, `dpareto`, `dhalfnorm`, and `dhalfcauchy`; discrete and linked
#' distributions `dbern`, `bernoulli_logit`, `bernoulli_probit`,
#' `bernoulli_cloglog`, `dbinom`, `binomial_logit`, `dpois`, `poisson_log`,
#' `dnbinom`, and `dnbinom_log`; and multivariate/matrix distributions `dbvn`,
#' `dmvn`, `dwish`, `dinvwish`, and `dlkjcorr2`.
#'
#' Built-in distributions do not limit the models that can be expressed.
#' Model-specific log-density calculations may be placed in a `func` chunk and
#' added directly to `target` using ordinary C expressions.
#'
#' @section Optional distribution lookup cache:
#' Setting `log_cache = TRUE` enables an optional lookup-table approximation for
#' selected repeated elementary calculations. Unlike persistent deterministic
#' caches, this can perturb the numerical log target slightly and is therefore
#' disabled by default. `log_cache_bits` controls table resolution and the
#' associated memory/speed tradeoff. Use this option only when its approximation
#' is acceptable for the application.
#'
#' @param model A character string containing hobbs model source or a path to a
#'   `.c` model file. Modern model source can contain `param` and `dparam`
#'   declarations, `func` chunks, `block` declarations, probability statements,
#'   and attached `cache`/`update` declarations. A continuous declaration may
#'   end in `save=mean` to retain only its post-warmup mean. One-based ascending
#'   loops written as `for (i in 1:N)` or `for (i = 1:N)` are translated to C
#'   loops before compilation.
#' @param dim Optional total parameter dimension. Normally inferred from `param`
#'   and `dparam` declarations. For legacy models without declarations, hobbs
#'   also looks for a scalar in `data` named `dim`, `theta_dim`, `npar`,
#'   `n_params`, or `param_dim`.
#' @param data Optional model data. Usually a named R list containing numeric,
#'   integer, or logical scalars, vectors, and matrices. Scalars are exposed by
#'   name; vectors and matrices receive one-based function-like accessors such
#'   as `y(i)` and `X(i, j)`. Vector lengths are exposed as `<name>_len`; matrix
#'   dimensions are exposed as `<name>_len`, `<name>_nrow`, and `<name>_ncol`.
#'   Advanced models may instead supply a path consumed by
#'   `posterior_init(const char*)`.
#' @param samples Number of retained post-warmup samples to save. Default
#'   `1000`.
#' @param burnin Number of discarded warmup iterations. Default `500`.
#'   `warmups` is the preferred alias; do not supply both.
#' @param adapt_until Last iteration at which continuous proposal scales are
#'   adapted. By default this equals the effective warmup length.
#' @param out Output path. By default, `chain.bin` in `workdir`. When
#'   `save = "mean"` and `out` is omitted, the default becomes
#'   `posterior_mean.csv`.
#' @param workdir Working directory used for generated model source, data
#'   bindings, compiled libraries, and other temporary build products. Defaults
#'   to [tempdir()].
#' @param binary Optional path to an existing hobbs Rust sampler executable.
#'   When `NULL`, the bundled sampler is built or reused from the hobbs user
#'   cache.
#' @param eval Evaluation mode for legacy/full-posterior interfaces: one of
#'   `"auto"`, `"scalar"`, or `"batch"`. The default automatically uses an
#'   available evaluation export.
#' @param format Retained-chain output format, either `"bin"` or `"csv"`.
#'   Binary output is the default and is recommended for large chains.
#' @param save Global storage mode. `"chain"` retains full draws except for
#'   declarations marked `save=mean`; `"mean"` retains posterior means for all
#'   parameters and is the legacy all-parameter mean mode.
#' @param update Update mode. `"block"` (the default) uses parameter-local
#'   scalar blocks and their attached cache updates. `"global"` performs
#'   one-coordinate proposals against a full-posterior evaluation.
#' @param rows_by Optional named list defining indexed row maps for local blocks.
#'   For example, `rows_by = list(ability = ability_idx)` creates
#'   `ability_nrows(p)` and `ability_row(p, ii)` accessors in generated C. This
#'   is useful when a parameter coordinate affects an irregular subset of rows.
#' @param no_output Logical. If `TRUE`, run the sampler without writing retained
#'   output, primarily for benchmarking.
#' @param step Positive initial Gaussian random-walk proposal scale for
#'   continuous coordinates. Default `0.25`; coordinate-specific scales are
#'   adapted during warmup.
#' @param adapt_every Retained for backward compatibility. Scalar Robbins-Monro
#'   adaptation is performed every warmup sweep.
#' @param target_accept Target acceptance probability for continuous scalar
#'   Metropolis proposals. If `NULL`, defaults to `0.44`.
#' @param seed RNG seed. A numeric seed may be any exactly representable whole
#'   number from 0 through `2^53 - 1`; a decimal character string may be used
#'   for the full unsigned 64-bit seed range.
#' @param thin Save every `thin`th post-warmup draw. Default `1`.
#' @param quiet Logical. If `TRUE`, suppress sampler/build command output.
#' @param rebuild_sampler Logical. If `TRUE`, force a rebuild of the bundled
#'   Rust sampler before running the model.
#' @param compiler Optional C compiler command. By default hobbs detects a
#'   suitable compiler from the system toolchain.
#' @param cflags Optional character vector or string of additional C compiler
#'   flags used when compiling the generated model.
#' @param log_cache Logical. If `TRUE`, enable the optional lookup-table
#'   approximation for selected repeated log/distribution calculations. Default
#'   `FALSE` because this can slightly perturb the log target.
#' @param log_cache_bits Integer table-resolution setting used when
#'   `log_cache = TRUE`. The default `8` allocates `2^8` mantissa lookup values
#'   (about 2 KB for the log table). Larger values increase resolution and cache
#'   footprint.
#' @param warmups Optional preferred alias for `burnin`. When supplied, do not
#'   also supply `burnin`. If `adapt_until` is omitted, adaptation defaults to
#'   the same number of iterations.
#'
#' @return Invisibly returns an object of class `hobbs_run`. Important elements
#'   include `output`, `chain_output`, and `mean_output`; generated model paths
#'   (`model_c`, `user_model_c`, `model_lib`); parameter dimensions and names;
#'   `samples`, `warmups`, and `adapt_until`; block and adaptation metadata; and
#'   the process `status`. Full-chain and mean-only outputs can be read with
#'   [read_hobbs()] and [read_hobbs_mean()], respectively.
#'
#' @seealso [read_hobbs()], [read_hobbs_mean()], [hobbs_check_toolchain()],
#'   [hobbs_install_sampler()], [hobbs_check_sampler()]
#'
#' @examples
#' \dontrun{
#' library(hobbs)
#'
#' set.seed(1)
#' n <- 200L
#' p <- 4L
#' X <- cbind(1, matrix(rnorm(n * (p - 1L)), nrow = n))
#' beta_true <- c(0.5, 1, -0.75, 0.25)
#' sigma_true <- 0.75
#' y <- as.numeric(X %*% beta_true + rnorm(n, sd = sigma_true))
#'
#' dat <- list(n = n, p = p, X = X, y = y)
#'
#' model <- '
#' param beta(p);
#' param logsigma(1);
#'
#' func llk() {
#'   double sigma = exp(logsigma(1));
#'   for (i = 1:n) {
#'     y(i) ~ dnorm(mu(i), sigma);
#'   }
#' }
#'
#' block beta(j) {
#'   beta(j) ~ dnorm(0, 10);
#'   llk();
#' } cache mu(n) {
#'   for (i = 1:n) {
#'     for (k = 1:p) {
#'       mu(i) += beta(k) * X(i, k);
#'     }
#'   }
#' } update mu(n) {
#'   for (i = 1:n) {
#'     mu(i) += (proposal(beta(j)) - current(beta(j))) * X(i, j);
#'   }
#' }
#'
#' block logsigma(1) {
#'   logsigma(1) ~ dnorm(0, 2);
#'   llk();
#' }
#' '
#'
#' fit <- hobbs(
#'   model = model,
#'   data = dat,
#'   samples = 2000,
#'   warmups = 1000,
#'   seed = 123,
#'   out = "regression.bin"
#' )
#'
#' draws <- read_hobbs(fit)
#' colMeans(draws[paste0("beta[", seq_len(p), "]")])
#' }
#' @export

hobbs <- function(model,
                    dim = NULL,
                    data = NULL,
                    samples = 1000L,
                    burnin = 500L,
                    adapt_until = burnin,
                    out = file.path(workdir, "chain.bin"),
                    workdir = tempdir(),
                    binary = NULL,
                    eval = c("auto", "scalar", "batch"),
                    format = c("bin", "csv"),
                    save = c("chain", "mean"),
                    update = c("block", "global"),
                    rows_by = NULL,
                    no_output = FALSE,
                    step = 0.25,
                    adapt_every = 25L,
                    target_accept = NULL,
                    seed = 123456789,
                    thin = 1L,
                    quiet = FALSE,
                    rebuild_sampler = FALSE,
                    compiler = NULL,
                    cflags = NULL,
                    log_cache = FALSE,
                    log_cache_bits = 8L,
                    warmups = NULL) {
  eval <- match.arg(eval)
  format <- match.arg(format)
  save <- match.arg(save)
  update <- match.arg(update)

  burnin_missing <- missing(burnin)
  adapt_until_missing <- missing(adapt_until)

  count_arg <- function(value, name, minimum) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < minimum || value != floor(value) || value > .Machine$integer.max) {
      stop(name, " must be a whole number between ", minimum,
           " and ", .Machine$integer.max, call. = FALSE)
    }
    as.integer(value)
  }
  samples <- count_arg(samples, "samples", 1L)
  if (!is.null(warmups)) {
    if (!burnin_missing) {
      stop("Specify only one of `warmups` and `burnin`.", call. = FALSE)
    }
    warmups <- count_arg(warmups, "warmups", 0L)
    burnin <- warmups
    if (adapt_until_missing) adapt_until <- warmups
  }
  burnin <- count_arg(burnin, "burnin", 0L)
  warmups <- burnin
  adapt_until <- count_arg(adapt_until, "adapt_until", 0L)
  thin <- count_arg(thin, "thin", 1L)
  adapt_every <- count_arg(adapt_every, "adapt_every", 1L)
  if (!is.numeric(step) || length(step) != 1L || !is.finite(step) || step <= 0) {
    stop("step must be a positive finite number", call. = FALSE)
  }
  seed_arg <- format_hobbs_seed(seed)

  if (identical(save, "mean") && missing(out)) {
    out <- file.path(workdir, "posterior_mean.csv")
    format <- "csv"
  }
  if (is.null(data) && is.list(dim) && !is.null(names(dim))) {
    # Convenience for hobbs(model, data = list(...)) accidentally supplied as
    # the second positional argument after dim became optional.
    data <- dim
    dim <- NULL
  }
  target_accept_user <- target_accept
  log_cache_config <- validate_log_cache(log_cache, log_cache_bits)

  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  workdir <- normalizePath(workdir, mustWork = TRUE)

  if (is.null(binary)) binary <- hobbs_build_sampler(rebuild = rebuild_sampler, quiet = quiet)
  binary <- normalizePath(binary, mustWork = TRUE)

  model_user_c <- materialize_model(model, workdir)
  model_user_c <- inline_func_declarations(model_user_c, workdir)
  data <- add_implicit_data_dimensions(data)
  param_info <- parse_param_declarations(model_user_c, data = data)
  model_runtime_c <- model_user_c
  block_info <- parse_block_declarations(model_runtime_c)
  dim <- resolve_dim(dim, data, param_info = param_info)
  output_plan <- resolve_parameter_output_plan(param_info, dim, global_save = save)
  data <- materialize_rows_by_data(data, rows_by, param_info = param_info)
  data_info <- materialize_data(data, workdir, row_map_spec = attr(data, "hobbs_row_map_spec", exact = TRUE))
  block_runtime <- resolve_block_runtime(update, param_info, block_info)

  if (is.null(target_accept_user)) {
    target_accept <- 0.44
  } else {
    target_accept <- target_accept_user
  }
  if (!is.numeric(target_accept) || length(target_accept) != 1L || !is.finite(target_accept) ||
      target_accept <= 0 || target_accept >= 1) {
    stop("target_accept must be NULL or a number between 0 and 1", call. = FALSE)
  }

  model_c <- prepare_model_translation_unit(
    model_runtime_c,
    workdir = workdir,
    log_cache = log_cache_config$enabled,
    data_spec = data_info$spec,
    param_info = param_info,
    block_info = block_info,
    allow_block_only = identical(update, "block")
  )
  lib <- compile_c_model(model_c, workdir = workdir, compiler = compiler, cflags = cflags,
                         quiet = quiet, log_cache = log_cache_config)

  block_cli_args <- unlist(lapply(block_runtime, function(b) {
    value_type <- b$value_type %||% "continuous"
    if (identical(value_type, "discrete")) {
      spec <- paste(b$name, b$offset, b$len, b$type, value_type, b$lower, b$upper, sep = ":")
    } else {
      # Keep continuous blocks in the older 4-field form.  The Rust parser
      # only needs bounds for discrete blocks, and passing NA bounds for
      # continuous blocks caused `--block lower must be an integer`.
      spec <- paste(b$name, b$offset, b$len, b$type, sep = ":")
    }
    c("--block", spec)
  }), use.names = FALSE)
  mean_range_cli_args <- if (identical(save, "chain") && output_plan$declaration_means) {
    unlist(lapply(output_plan$mean_ranges, function(item) {
      c("--mean-range", paste(item$offset, item$len, sep = ":"))
    }), use.names = FALSE)
  } else {
    character()
  }

  args <- c(
    "--lib", lib,
    if (!is.null(data_info$path)) c("--data", data_info$path),
    "--dim", as.integer(dim),
    "--samples", samples,
    "--burnin", burnin,
    "--adapt-until", adapt_until,
    "--eval", eval,
    "--format", format,
    "--save", save,
    "--update", update,
    block_cli_args,
    mean_range_cli_args,
    "--step", format_num(step),
    "--adapt-every", adapt_every,
    "--target-accept", format_num(target_accept),
    "--seed", seed_arg,
    "--thin", thin,
    if (isTRUE(quiet)) "--quiet"
  )

  adaptation_path <- NA_character_
  mean_output_path <- NA_character_
  chain_output_path <- NA_character_
  declaration_means <- identical(save, "chain") && isTRUE(output_plan$declaration_means)

  if (isTRUE(no_output)) {
    args <- c(args, "--no-output")
    out_path <- NA_character_
  } else {
    out_path <- normalizePath(dirname(out), mustWork = TRUE)
    out_path <- file.path(out_path, basename(out))
    args <- c(args, "--out", out_path)

    if (declaration_means) {
      # Mixed output keeps `out` as the ordinary chain. If every declaration is
      # mean-only, the one-row binary naturally becomes the primary output.
      mean_output_path <- if (output_plan$chain_dim == 0L) {
        out_path
      } else {
        paste0(tools::file_path_sans_ext(out_path), ".mean.bin")
      }
      args <- c(args, "--mean-out", mean_output_path)
      if (output_plan$chain_dim > 0L) chain_output_path <- out_path
    } else if (identical(save, "mean")) {
      mean_output_path <- out_path
    } else {
      chain_output_path <- out_path
    }

    if (identical(update, "block")) {
      adaptation_path <- paste0(out_path, ".adaptation.csv")
      args <- c(args, "--adapt-diagnostics-out", adaptation_path)
    }
  }

  stdout <- if (quiet) FALSE else ""
  stderr <- if (quiet) FALSE else ""
  # system2() on Unix can still split unquoted arguments that contain spaces
  # in some environments, so quote command arguments defensively.
  res <- system2(binary, shQuote(args), stdout = stdout, stderr = stderr)
  if (!identical(res, 0L)) stop("hobbs sampler failed with status ", res, call. = FALSE)
  if (!isTRUE(no_output) && !file.exists(out_path)) {
    stop("hobbs sampler reported success but did not create output file: ", out_path,
         ". This usually means an old cached sampler binary was used; retry with rebuild_sampler = TRUE.",
         call. = FALSE)
  }
  if (!isTRUE(no_output) && declaration_means && !file.exists(mean_output_path)) {
    stop("hobbs sampler did not create declaration-level posterior means: ",
         mean_output_path, call. = FALSE)
  }
  if (!isTRUE(no_output) && identical(update, "block")) {
    if (!file.exists(adaptation_path)) {
      stop("hobbs sampler did not create scalar adaptation diagnostics: ",
           adaptation_path, call. = FALSE)
    }
  }

  parameter_save <- if (length(param_info$spec)) {
    modes <- if (identical(save, "mean")) {
      rep("mean", length(param_info$spec))
    } else {
      vapply(param_info$spec, function(item) item$save %||% "chain", character(1))
    }
    setNames(modes, vapply(param_info$spec, `[[`, character(1), "name"))
  } else {
    character()
  }

  if (!isTRUE(no_output)) {
    if (identical(save, "mean")) {
      primary_dim <- as.integer(dim)
      primary_names <- param_info$names
      primary_save <- "mean"
      primary_format <- "csv"
      primary_has_binary_header <- FALSE
    } else if (declaration_means && output_plan$chain_dim == 0L) {
      primary_dim <- output_plan$mean_dim
      primary_names <- output_plan$mean_names
      primary_save <- "mean"
      primary_format <- "bin"
      primary_has_binary_header <- TRUE
    } else {
      primary_dim <- output_plan$chain_dim
      primary_names <- output_plan$chain_names
      primary_save <- "chain"
      primary_format <- format
      primary_has_binary_header <- identical(format, "bin")
    }

    if (length(primary_names)) {
      saveRDS(primary_names, paste0(out_path, ".param_names.rds"))
    }
    metadata <- list(
      dim = as.integer(primary_dim),
      model_dim = as.integer(dim),
      param_names = primary_names,
      all_param_names = param_info$names,
      chain_param_names = output_plan$chain_names,
      mean_param_names = output_plan$mean_names,
      chain_output = chain_output_path,
      mean_output = mean_output_path,
      parameter_save = parameter_save,
      samples = as.integer(samples),
      burnin = as.integer(burnin),
      warmups = as.integer(warmups),
      adapt_until = as.integer(adapt_until),
      adaptation = adaptation_path,
      target_accept = target_accept,
      step = step,
      seed = seed_arg,
      thin = as.integer(thin),
      update = update,
      format = primary_format,
      save = primary_save,
      has_binary_header = primary_has_binary_header,
      record_size = if (primary_has_binary_header) {
        as.integer(24L + 8L * as.integer(primary_dim))
      } else {
        NA_integer_
      }
    )
    saveRDS(metadata, paste0(out_path, ".metadata.rds"))

    if (declaration_means && output_plan$chain_dim > 0L) {
      if (length(output_plan$mean_names)) {
        saveRDS(output_plan$mean_names, paste0(mean_output_path, ".param_names.rds"))
      }
      mean_metadata <- list(
        dim = output_plan$mean_dim,
        model_dim = as.integer(dim),
        param_names = output_plan$mean_names,
        all_param_names = param_info$names,
        source_output = out_path,
        retained_samples = as.integer(samples),
        format = "bin",
        save = "mean",
        has_binary_header = TRUE,
        record_size = as.integer(24L + 8L * output_plan$mean_dim)
      )
      saveRDS(mean_metadata, paste0(mean_output_path, ".metadata.rds"))
    }
  }

  ans <- list(
    output = out_path,
    chain_output = chain_output_path,
    mean_output = mean_output_path,
    model_c = model_c,
    user_model_c = model_user_c,
    runtime_model_c = model_runtime_c,
    model_lib = lib,
    data = data_info$path,
    data_spec = data_info$spec,
    binary = binary,
    dim = as.integer(dim),
    chain_dim = output_plan$chain_dim,
    mean_dim = output_plan$mean_dim,
    param_names = param_info$names,
    chain_param_names = output_plan$chain_names,
    mean_param_names = output_plan$mean_names,
    parameter_save = parameter_save,
    param_spec = param_info$spec,
    samples = as.integer(samples),
    burnin = as.integer(burnin),
    warmups = as.integer(warmups),
    adapt_until = as.integer(adapt_until),
    adaptation = adaptation_path,
    target_accept = target_accept,
    step = step,
    seed = seed_arg,
    eval = eval,
    format = format,
    save = save,
    update = update,
    blocks = block_runtime,
    no_output = isTRUE(no_output),
    log_cache = log_cache_config,
    status = res
  )
  class(ans) <- "hobbs_run"
  invisible(ans)
}


format_hobbs_seed <- function(seed) {
  if (is.character(seed)) {
    if (length(seed) != 1L || is.na(seed) || !grepl("^[0-9]+$", seed, perl = TRUE)) {
      stop("seed must be a non-negative whole number", call. = FALSE)
    }
    seed <- sub("^0+(?=[0-9])", "", seed, perl = TRUE)
    # u64::MAX = 18446744073709551615. Compare decimal strings without losing
    # precision in R's double representation.
    max_u64 <- "18446744073709551615"
    if (nchar(seed) > nchar(max_u64) ||
        (nchar(seed) == nchar(max_u64) && seed > max_u64)) {
      stop("seed must fit in an unsigned 64-bit integer", call. = FALSE)
    }
    return(seed)
  }

  # Numeric R values are exact only through 2^53 - 1. Character seeds provide
  # access to the complete unsigned 64-bit range.
  max_exact <- 9007199254740991
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed < 0 ||
      seed != floor(seed) || seed > max_exact) {
    stop(
      "numeric seed must be a whole number from 0 through 2^53 - 1; use a decimal character string for larger unsigned 64-bit seeds",
      call. = FALSE
    )
  }
  sprintf("%.0f", seed)
}

#' Read hobbs binary output
#'
#' Reads the default binary chain. When an `hobbs_run` uses declaration-level
#' `save=mean`, this returns the full draws for the unmarked parameters. Use
#' [read_hobbs_mean()] for the one-row binary of mean-only parameters.
#'
#' @param file Path to binary chain output.
#' @param dim Number of parameters in the chain. If omitted, it is read from
#'   the binary header written by recent hobbs versions.
#' @param max_records Optional maximum number of records to read.
#' @param param_names Optional names for theta columns. Usually supplied automatically when reading a `hobbs_run` object returned by `hobbs()`.
#' @return A data frame with columns `iter`, `accepted`, `logp`, and the saved parameter columns.
#' @export
read_hobbs <- function(file, dim = NULL, max_records = NULL, param_names = NULL) {
  if (inherits(file, "hobbs_run")) {
    has_chain_output <- !is.null(file$chain_output) && length(file$chain_output) == 1L &&
      !is.na(file$chain_output) && nzchar(file$chain_output)
    has_mean_output <- !is.null(file$mean_output) && length(file$mean_output) == 1L &&
      !is.na(file$mean_output) && nzchar(file$mean_output)

    if (identical(file$save, "mean") || (!has_chain_output && has_mean_output)) {
      return(read_hobbs_mean(file, param_names = param_names))
    }
    if (is.null(param_names)) {
      param_names <- file$chain_param_names %||% file$param_names
    }
    if (is.null(dim)) dim <- file$chain_dim %||% file$dim
    file <- if (has_chain_output) file$chain_output else file$output
  }
  file <- normalizePath(file, mustWork = TRUE)

  metadata_path <- paste0(file, ".metadata.rds")
  if (file.exists(metadata_path)) {
    metadata <- readRDS(metadata_path)
    if (is.null(dim) && !is.null(metadata$dim)) dim <- metadata$dim
    if (is.null(param_names) && !is.null(metadata$param_names)) param_names <- metadata$param_names
  }

  if (is.null(param_names)) {
    sidecar <- paste0(file, ".param_names.rds")
    if (file.exists(sidecar)) param_names <- readRDS(sidecar)
  }
  if (is.null(dim) && !is.null(param_names)) {
    dim <- length(param_names)
  }

  if (is.null(max_records)) max_records <- -1L
  dim_arg <- if (is.null(dim)) -1L else as.integer(dim)
  out <- .Call("hobbs_read_bin", file, dim_arg, as.integer(max_records), PACKAGE = "hobbs")
  dim_out <- length(out) - 3L
  if (!is.null(param_names)) {
    if (length(param_names) != dim_out) {
      stop("`param_names` length must match the number of theta columns in the chain.", call. = FALSE)
    }
    theta_names <- param_names
  } else {
    theta_names <- paste0("theta", seq_len(dim_out))
  }
  names(out) <- c("iter", "accepted", "logp", theta_names)
  class(out) <- "data.frame"
  attr(out, "row.names") <- .set_row_names(length(out[[1L]]))
  out
}


is_hobbs_binary_file <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  magic <- readBin(con, what = "raw", n = 12L)
  length(magic) == 12L && identical(rawToChar(magic), "hobbs_BIN_V1")
}


#' Read hobbs posterior-mean output
#'
#' For declaration-level `save=mean`, reads the one-record standard hobbs
#' binary written beside the ordinary chain. The `saved` field is the number of
#' retained draws used in each posterior mean. The older global
#' `hobbs(save = "mean")` CSV format remains readable for compatibility.
#'
#' @param file An `hobbs_run` object or path to a mean output file.
#' @param param_names Optional parameter names. Usually supplied automatically.
#' @return A one-row data frame with `saved`, `logp`, and posterior mean columns.
#' @export
read_hobbs_mean <- function(file, param_names = NULL) {
  dim <- NULL
  if (inherits(file, "hobbs_run")) {
    has_mean_output <- !is.null(file$mean_output) && length(file$mean_output) == 1L &&
      !is.na(file$mean_output) && nzchar(file$mean_output)
    if (is.null(param_names)) {
      param_names <- if (length(file$mean_param_names %||% character())) {
        file$mean_param_names
      } else {
        file$param_names
      }
    }
    dim <- if (has_mean_output && !is.null(file$mean_dim) && file$mean_dim > 0L) {
      file$mean_dim
    } else {
      file$dim
    }
    file <- if (has_mean_output) file$mean_output else file$output
  }
  file <- normalizePath(file, mustWork = TRUE)
  if (is.null(param_names)) {
    sidecar <- paste0(file, ".param_names.rds")
    if (file.exists(sidecar)) param_names <- readRDS(sidecar)
  }

  if (is_hobbs_binary_file(file)) {
    if (is.null(dim) && !is.null(param_names)) dim <- length(param_names)
    out <- read_hobbs(
      file,
      dim = dim,
      param_names = param_names
    )
    if (nrow(out) != 1L) {
      stop("Posterior-mean binary must contain exactly one record.", call. = FALSE)
    }
    out$accepted <- NULL
    names(out)[names(out) == "iter"] <- "saved"
    return(out)
  }

  out <- utils::read.csv(file, check.names = FALSE)
  theta_cols <- grep("^theta[0-9]+$", names(out), value = TRUE)
  if (!is.null(param_names) && length(param_names)) {
    if (length(param_names) != length(theta_cols)) {
      stop("`param_names` length must match the number of theta columns in the mean output.", call. = FALSE)
    }
    names(out)[match(theta_cols, names(out))] <- param_names
  }
  out
}


#' Write an example C posterior model
#'
#' @param path Destination path.
#' @param batch Logical. If TRUE, write a batch-capable model.
#' @return Path to the written C file.
#' @export
hobbs_example_model <- function(path = tempfile(fileext = ".c"), batch = TRUE) {
  code <- if (isTRUE(batch)) example_batch_code() else example_scalar_code()
  writeLines(code, path)
  normalizePath(path, mustWork = TRUE)
}

run_in_dir <- function(dir, command, args = character(), stdout = "", stderr = "") {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(dir)
  system2(command, args, stdout = stdout, stderr = stderr)
}

hobbs_cache_dir <- function() {
  if (utils::packageVersion("utils") >= "3.6.0") {
    tools::R_user_dir("hobbs", which = "cache")
  } else {
    file.path(path.expand("~"), ".cache", "hobbs")
  }
}

exe_name <- function(x) {
  if (.Platform$OS.type == "windows") paste0(x, ".exe") else x
}

hobbs_rust_target <- function() {
  if (.Platform$OS.type != "windows") return(NULL)

  if (grepl("aarch", R.version$platform, ignore.case = TRUE)) {
    "aarch64-pc-windows-gnullvm"
  } else if (grepl("clang", Sys.getenv("R_COMPILED_BY"), ignore.case = TRUE)) {
    "x86_64-pc-windows-gnullvm"
  } else if (grepl("i386", R.version$platform, ignore.case = TRUE)) {
    "i686-pc-windows-gnu"
  } else {
    "x86_64-pc-windows-gnu"
  }
}

hobbs_sampler_executable <- function(root, rust_target = hobbs_rust_target()) {
  release_dir <- if (is.null(rust_target)) {
    file.path(root, "target", "release")
  } else {
    file.path(root, "target", rust_target, "release")
  }
  file.path(release_dir, exe_name("hobbs"))
}

ensure_rust_target <- function(target, quiet = TRUE) {
  if (is.null(target)) return(invisible(TRUE))

  rustup <- Sys.which("rustup")
  if (!nzchar(rustup)) return(invisible(FALSE))

  installed <- suppressWarnings(
    system2(rustup, c("target", "list", "--installed"), stdout = TRUE, stderr = TRUE)
  )
  status <- attr(installed, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) return(invisible(FALSE))
  if (target %in% trimws(installed)) return(invisible(TRUE))

  out <- if (quiet) FALSE else ""
  err <- if (quiet) FALSE else ""
  status <- system2(rustup, c("target", "add", target), stdout = out, stderr = err)
  if (!identical(status, 0L)) {
    stop("Could not install the required Rust target `", target, "`.", call. = FALSE)
  }

  invisible(TRUE)
}

copy_dir <- function(from, to) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE), to, recursive = TRUE)
  if (!all(ok)) stop("Failed to copy bundled hobbs source into cache", call. = FALSE)
}

materialize_model <- function(model, workdir) {
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    stop("`model` must be a single path or a single C/hobbs source string", call. = FALSE)
  }

  # Existing paths keep the traditional file-based workflow.  Everything else
  # is interpreted as inline model source only if it looks like C/hobbs code.
  if (file.exists(model)) return(normalizePath(model, mustWork = TRUE))

  if (!looks_like_model_source(model)) {
    stop("`model` is neither an existing file nor inline hobbs model source. ",
         "Pass a file path, or a string containing declarations such as `param`, `block`, ",
         "or a model body with `{ ... }`.",
         call. = FALSE)
  }

  path <- file.path(workdir, paste0("hobbs_inline_model_",
                                    format(Sys.time(), "%Y%m%d%H%M%OS3"),
                                    "_", sprintf("%06d", sample.int(999999L, 1L)),
                                    ".c"))
  writeLines(model, path, useBytes = TRUE)
  normalizePath(path, mustWork = TRUE)
}

looks_like_model_source <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)
  if (grepl("\n|\r", x)) return(TRUE)
  if (grepl("[{};]", x)) return(TRUE)
  if (grepl("\b(param|dparam|block|func|for|return|double|int|posterior_logp|log_posterior)\b", x, perl = TRUE)) return(TRUE)
  FALSE
}


inline_func_declarations <- function(model_c, workdir) {
  src <- readLines(model_c, warn = FALSE)
  parsed <- extract_zero_arg_funcs(src)
  if (!length(parsed$funcs)) return(model_c)
  expanded <- inline_zero_arg_func_calls(parsed$src, parsed$funcs)
  out <- file.path(workdir, "hobbs_model_func_inlined.c")
  writeLines(expanded, out)
  normalizePath(out, mustWork = TRUE)
}

extract_zero_arg_funcs <- function(src) {
  funcs <- list()
  keep <- character()
  i <- 1L
  pat <- "^([[:space:]]*)func[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*\\)[[:space:]]*\\{(.*)$"
  while (i <= length(src)) {
    line <- src[[i]]
    m <- regexec(pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 4L) {
      nm <- hit[[3L]]
      if (nm %in% names(funcs)) stop("Duplicate func definition: ", nm, call. = FALSE)
      block_lines <- line
      bal <- count_char(line, "{") - count_char(line, "}")
      j <- i
      while (bal > 0L && j < length(src)) {
        j <- j + 1L
        block_lines <- c(block_lines, src[[j]])
        bal <- bal + count_char(src[[j]], "{") - count_char(src[[j]], "}")
      }
      if (bal != 0L) stop("Unbalanced braces in func definition `", nm, "`.", call. = FALSE)
      funcs[[nm]] <- extract_func_body_lines(block_lines)
      i <- j + 1L
      next
    }
    if (grepl("^[[:space:]]*func[[:space:]]+", line, perl = TRUE)) {
      stop("Invalid func definition. Use zero-argument syntax like `func name() { ... }`.", call. = FALSE)
    }
    keep <- c(keep, line)
    i <- i + 1L
  }
  list(src = keep, funcs = funcs)
}

extract_func_body_lines <- function(block_lines) {
  if (!length(block_lines)) return(character())
  first <- block_lines[[1L]]
  last <- block_lines[[length(block_lines)]]
  first_after <- sub("^[^{]*\\{", "", first, perl = TRUE)
  if (length(block_lines) == 1L) {
    inner <- sub("}[^}]*$", "", first_after, perl = TRUE)
    return(if (nzchar(trimws(inner))) inner else character())
  }
  body <- character()
  if (nzchar(trimws(first_after))) body <- c(body, first_after)
  if (length(block_lines) > 2L) body <- c(body, block_lines[2L:(length(block_lines) - 1L)])
  last_before <- sub("}[[:space:]]*(//.*)?$", "", last, perl = TRUE)
  if (nzchar(trimws(last_before))) body <- c(body, last_before)
  body
}

inline_zero_arg_func_calls <- function(src, funcs) {
  out <- character()
  call_names <- names(funcs)
  if (!length(call_names)) return(src)
  call_pat <- paste0("^([[:space:]]*)(", paste(call_names, collapse = "|"), ")[[:space:]]*\\([[:space:]]*\\)[[:space:]]*;[[:space:]]*(?://.*)?$")
  for (line in src) {
    m <- regexec(call_pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 3L) {
      indent <- hit[[2L]]
      nm <- hit[[3L]]
      body <- funcs[[nm]]
      # `func` declarations are raw code chunks.  Inline the body exactly at
      # the call site, preserving the caller's scope rather than adding an
      # extra C block that would hide declarations made by the chunk.
      if (length(body)) out <- c(out, paste0(indent, body))
    } else {
      out <- c(out, line)
    }
  }
  if (any(grepl(call_pat, out, perl = TRUE))) {
    out2 <- inline_zero_arg_func_calls(out, funcs)
    if (!identical(out2, out)) return(out2)
  }
  out
}



resolve_dim <- function(dim, data = NULL, param_info = NULL) {
  if (!is.null(dim)) {
    if (length(dim) != 1L || is.na(dim) || !is.finite(dim) || dim <= 0) {
      stop("`dim` must be a single positive integer.", call. = FALSE)
    }
    dim <- as.integer(dim)
    if (!is.null(param_info) && !is.null(param_info$dim) && param_info$dim > 0L && dim != as.integer(param_info$dim)) {
      stop("`dim` does not match the total size implied by `param ...` declarations: ",
           dim, " vs ", as.integer(param_info$dim), ".", call. = FALSE)
    }
    return(dim)
  }

  if (!is.null(param_info) && !is.null(param_info$dim) && is.finite(param_info$dim) && param_info$dim > 0) {
    return(as.integer(param_info$dim))
  }

  if (is.list(data) && !is.null(names(data))) {
    candidates <- c("dim", "theta_dim", "npar", "n_params", "param_dim")
    for (nm in candidates) {
      if (nm %in% names(data)) {
        x <- data[[nm]]
        if ((is.numeric(x) || is.integer(x)) && length(x) == 1L && is.finite(x) && x > 0) {
          return(as.integer(x))
        }
      }
    }
  }

  stop("`dim` was not supplied. Either pass `dim = ...`, add `param name(k)` declarations to the C model, or include an explicit scalar such as `theta_dim = ...` in `data`.",
       call. = FALSE)
}

resolve_parameter_output_plan <- function(param_info, dim, global_save = "chain") {
  all_names <- param_info$names %||% character()
  if (identical(global_save, "mean")) {
    return(list(
      chain_dim = 0L, mean_dim = as.integer(dim),
      chain_names = character(), mean_names = all_names,
      mean_ranges = list(), declaration_means = FALSE
    ))
  }

  ranges <- param_info$mean_ranges %||% list()
  mean_dim <- if (length(ranges)) {
    sum(vapply(ranges, function(item) as.integer(item$len), integer(1)))
  } else {
    0L
  }
  if (mean_dim < 0L || mean_dim > dim) {
    stop("Internal parameter-output plan exceeds model dimension.", call. = FALSE)
  }
  list(
    chain_dim = as.integer(dim - mean_dim),
    mean_dim = as.integer(mean_dim),
    chain_names = param_info$chain_names %||% all_names,
    mean_names = param_info$mean_names %||% character(),
    mean_ranges = ranges,
    declaration_means = length(ranges) > 0L
  )
}

materialize_data <- function(data, workdir, row_map_spec = NULL) {
  if (is.null(data)) return(list(path = NULL, spec = NULL))
  if (is.character(data) && length(data) == 1L) {
    if (!file.exists(data)) stop("data file does not exist: ", data, call. = FALSE)
    return(list(path = normalizePath(data, mustWork = TRUE), spec = NULL))
  }
  if (!is.list(data) || is.null(names(data)) || any(!nzchar(names(data)))) {
    stop("`data` must be NULL, a data file path, or a named list of numeric/integer/logical scalars, vectors, and matrices.", call. = FALSE)
  }

  data <- add_implicit_data_dimensions(data)

  if (anyDuplicated(names(data))) stop("`data` names must be unique.", call. = FALSE)
  c_names <- vapply(names(data), sanitize_c_identifier, character(1))
  if (anyDuplicated(c_names)) stop("`data` names must be unique after conversion to C identifiers.", call. = FALSE)

  specs <- vector("list", length(data))
  payload_path <- file.path(workdir, "hobbs_data.bin")
  con <- file(payload_path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.integer(length(data)), con, size = 4L, endian = "little")

  for (i in seq_along(data)) {
    x <- data[[i]]
    nm <- names(data)[[i]]
    cn <- c_names[[i]]
    dims <- dim(x)
    is_matrix <- length(dims) == 2L
    is_scalar <- is.null(dims) && length(x) == 1L
    if (is.null(dims)) dims <- c(length(x), 1L)
    if (!length(x)) stop("data item `", nm, "` has length zero", call. = FALSE)
    if (!(is.numeric(x) || is.integer(x) || is.logical(x))) {
      stop("data item `", nm, "` must be numeric, integer, or logical", call. = FALSE)
    }
    type <- if (is.double(x)) "double" else "int"
    rank <- if (is_scalar) 0L else if (is_matrix) 2L else 1L
    nrow <- as.integer(dims[[1L]])
    ncol <- as.integer(dims[[2L]])
    storage <- if (is_matrix) as.vector(x) else as.vector(x)
    # Keep R's native column-major matrix layout. The generated matrix macro
    # uses the automatically exposed <name>_nrow scalar to index it naturally.
    writeBin(as.integer(if (identical(type, "double")) 1L else 2L), con, size = 4L, endian = "little")
    writeBin(as.integer(rank), con, size = 4L, endian = "little")
    writeBin(nrow, con, size = 4L, endian = "little")
    writeBin(ncol, con, size = 4L, endian = "little")
    if (identical(type, "double")) {
      writeBin(as.double(storage), con, size = 8L, endian = "little")
    } else {
      writeBin(as.integer(storage), con, size = 4L, endian = "little")
    }
    specs[[i]] <- list(name = nm, c_name = cn, type = type, rank = rank,
                       is_scalar = is_scalar, is_matrix = is_matrix,
                       nrow = nrow, ncol = ncol, len = as.integer(length(x)))
  }
  attr(specs, "hobbs_row_map_spec") <- row_map_spec
  list(path = normalizePath(payload_path, mustWork = TRUE), spec = specs, row_map_spec = row_map_spec)
}

add_implicit_data_dimensions <- function(data) {
  if (is.null(data) || !is.list(data) || is.null(names(data))) return(data)

  row_map_spec <- attr(data, "hobbs_row_map_spec", exact = TRUE)
  existing_c_names <- vapply(names(data), sanitize_c_identifier, character(1))
  extra <- list()
  extra_c_names <- character()

  add_dim <- function(name, value) {
    cn <- sanitize_c_identifier(name)
    if (cn %in% existing_c_names || cn %in% extra_c_names) return()
    extra[[name]] <<- as.integer(value)
    extra_c_names <<- c(extra_c_names, cn)
  }

  for (i in seq_along(data)) {
    x <- data[[i]]
    nm <- names(data)[[i]]
    dims <- dim(x)
    is_scalar <- is.null(dims) && length(x) == 1L
    is_matrix <- length(dims) == 2L
    if (is_scalar) next

    base <- sanitize_c_identifier(nm)
    add_dim(paste0(base, "_len"), length(x))
    if (is_matrix) {
      add_dim(paste0(base, "_nrow"), dims[[1L]])
      add_dim(paste0(base, "_ncol"), dims[[2L]])
    }
  }

  if (length(extra)) data <- c(data, extra)
  attr(data, "hobbs_row_map_spec") <- row_map_spec
  data
}


materialize_rows_by_data <- function(data, rows_by = NULL, param_info = NULL) {
  if (is.null(rows_by)) return(data)
  if (!is.list(rows_by) || is.null(names(rows_by)) || any(!nzchar(names(rows_by)))) {
    stop("`rows_by` must be a named list, e.g. `rows_by = list(ability = ability_idx)`.", call. = FALSE)
  }
  if (is.null(param_info) || !length(param_info$spec)) {
    stop("`rows_by` requires `param ...` declarations in the C model.", call. = FALSE)
  }
  if (is.null(data)) data <- list()
  if (!is.list(data) || is.null(names(data))) {
    stop("`rows_by` requires `data` to be a named list.", call. = FALSE)
  }
  pmap <- setNames(param_info$spec, vapply(param_info$spec, `[[`, character(1), "name"))
  row_specs <- list()
  extra <- list()
  for (nm in names(rows_by)) {
    cn <- sanitize_c_identifier(nm)
    if (!(cn %in% names(pmap))) {
      stop("`rows_by` name `", nm, "` does not match a `param` block name.", call. = FALSE)
    }
    idx <- rows_by[[nm]]
    if (!(is.numeric(idx) || is.integer(idx)) || !length(idx)) {
      stop("`rows_by$", nm, "` must be a non-empty integer/numeric vector of 1-based block indices.", call. = FALSE)
    }
    idx <- as.integer(idx)
    n_block <- as.integer(pmap[[cn]]$len)
    if (any(is.na(idx)) || any(idx < 1L) || any(idx > n_block)) {
      stop("`rows_by$", nm, "` must contain 1-based indices from 1 to ", n_block, ".", call. = FALSE)
    }
    starts <- integer(n_block + 1L)
    rows <- integer(length(idx))
    pos <- 1L
    for (b in seq_len(n_block)) {
      starts[[b]] <- pos - 1L
      hits <- which(idx == b)
      if (length(hits)) {
        rows[pos:(pos + length(hits) - 1L)] <- as.integer(hits)
        pos <- pos + length(hits)
      }
    }
    starts[[n_block + 1L]] <- pos - 1L
    start_name <- paste0("hobbs_", cn, "_row_start")
    index_name <- paste0("hobbs_", cn, "_row_index")
    extra[[start_name]] <- as.integer(starts)
    extra[[index_name]] <- as.integer(rows)
    row_specs[[length(row_specs) + 1L]] <- list(name = cn, start = start_name, index = index_name, n_block = n_block)
  }
  dup <- intersect(names(extra), names(data))
  if (length(dup)) stop("Internal `rows_by` data names collide with user data: ", paste(dup, collapse = ", "), call. = FALSE)
  data <- c(data, extra)
  attr(data, "hobbs_row_map_spec") <- row_specs
  data
}

sanitize_c_identifier <- function(x) {
  y <- gsub("[^A-Za-z0-9_]", "_", x)
  y <- gsub("_+", "_", y)
  if (!grepl("^[A-Za-z_]", y)) y <- paste0("data_", y)
  y
}

generate_data_bridge_c <- function(spec, row_map_spec = NULL) {
  decl <- c(
    "#include <stdint.h>",
    "#include <stdio.h>",
    "#include <stdlib.h>",
    ""
  )

  for (item in spec) {
    cn <- item$c_name
    ctype <- if (identical(item$type, "double")) "double" else "int"
    decl <- c(decl, sprintf("/* explicit data item `%s` from hobbs(data = list(...)) */", cn))
    if (isTRUE(item$is_scalar)) {
      decl <- c(decl, sprintf("%s %s = 0;", ctype, cn))
    } else {
      decl <- c(decl, sprintf("%s *%s = NULL;", ctype, cn))
      if (isTRUE(item$is_matrix)) {
        # Matrix dimensions are exposed automatically as <name>_nrow,
        # <name>_ncol, and <name>_len. Matrix indexing is 1-based
        # to look like R code and uses R's
        # column-major memory layout: X(i,j) == R's X[i, j].
        decl <- c(decl,
          sprintf("#define %s(row, col) %s[((int)(row) - 1) + ((int)(col) - 1) * %s_nrow]", cn, cn, cn)
        )
      } else {
        # Vector indexing is also 1-based: y(i) == R's y[i].
        decl <- c(decl,
          sprintf("#define %s(i) %s[((int)(i) - 1)]", cn, cn)
        )
      }
    }
    decl <- c(decl, "")
  }

  if (!is.null(row_map_spec) && length(row_map_spec)) {
    decl <- c(decl, "/* generated indexed block row helpers */")
    for (rm in row_map_spec) {
      nm <- rm$name
      start <- paste0("hobbs_", nm, "_row_start")
      index <- paste0("hobbs_", nm, "_row_index")
      decl <- c(decl,
        sprintf("#define %s_nrows(block_index) (%s[(int)(block_index)] - %s[(int)(block_index) - 1])", nm, start, start),
        sprintf("#define %s_row(block_index, ii) %s[%s[(int)(block_index) - 1] + ((int)(ii) - 1)]", nm, index, start),
        ""
      )
    }
  }

  init <- c(
    "int posterior_init(const char *data_path) {",
    "    FILE *fp = fopen(data_path, \"rb\");",
    "    if (!fp) return 1;",
    "    int32_t n_items = 0;",
    "    if (fread(&n_items, sizeof(int32_t), 1, fp) != 1) { fclose(fp); return 2; }",
    sprintf("    if (n_items != %d) { fclose(fp); return 3; }", length(spec))
  )

  code <- 10L
  for (idx in seq_along(spec)) {
    item <- spec[[idx]]
    cn <- item$c_name
    ctype <- if (identical(item$type, "double")) "double" else "int"
    type_id <- if (identical(item$type, "double")) 1L else 2L
    rank <- item$rank
    nrow <- item$nrow
    ncol <- item$ncol
    len <- item$len
    init <- c(init,
      "    {",
      "        int32_t type_id = 0, rank = 0, nr = 0, nc = 0;",
      sprintf("        if (fread(&type_id, sizeof(int32_t), 1, fp) != 1) { fclose(fp); return %d; }", code),
      sprintf("        if (fread(&rank, sizeof(int32_t), 1, fp) != 1) { fclose(fp); return %d; }", code + 1L),
      sprintf("        if (fread(&nr, sizeof(int32_t), 1, fp) != 1) { fclose(fp); return %d; }", code + 2L),
      sprintf("        if (fread(&nc, sizeof(int32_t), 1, fp) != 1) { fclose(fp); return %d; }", code + 3L),
      sprintf("        if (type_id != %d || rank != %d || nr != %d || nc != %d) { fclose(fp); return %d; }", type_id, rank, nrow, ncol, code + 4L)
    )
    if (isTRUE(item$is_scalar)) {
      init <- c(init,
        sprintf("        if (fread(&%s, sizeof(%s), 1, fp) != 1) { fclose(fp); return %d; }", cn, ctype, code + 6L)
      )
    } else {
      init <- c(init,
        sprintf("        %s = (%s *)hobbs_aligned_malloc((size_t)%d * sizeof(%s));", cn, ctype, len, ctype),
        sprintf("        if (!%s) { fclose(fp); return %d; }", cn, code + 5L),
        sprintf("        if (fread(%s, sizeof(%s), (size_t)%d, fp) != (size_t)%d) { fclose(fp); return %d; }", cn, ctype, len, len, code + 6L)
      )
    }
    init <- c(init, "    }")
    code <- code + 10L
  }
  init <- c(init, "    fclose(fp);", "    return 0;", "}", "")

  free <- c("void posterior_free(void) {")
  for (item in spec) {
    if (!isTRUE(item$is_scalar)) {
      cn <- item$c_name
      free <- c(free,
        sprintf("    hobbs_aligned_free(%s);", cn),
        sprintf("    %s = NULL;", cn)
      )
    }
  }
  free <- c(free, "}")
  c(decl, init, free)
}



count_char <- function(x, ch) {
  if (!nzchar(x)) return(0L)
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  sum(chars == ch)
}

split_top_level_commas <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  if (!length(chars)) return(character())
  depth <- 0L
  start <- 1L
  out <- character()
  for (k in seq_along(chars)) {
    ch <- chars[[k]]
    if (identical(ch, "(")) depth <- depth + 1L
    else if (identical(ch, ")")) depth <- depth - 1L
    else if (identical(ch, ",") && depth == 0L) {
      out <- c(out, trimws(paste(chars[start:(k - 1L)], collapse = "")))
      start <- k + 1L
    }
  }
  out <- c(out, trimws(paste(chars[start:length(chars)], collapse = "")))
  out[nzchar(out)]
}


normalize_attached_cache_syntax <- function(src) {
  # The public deterministic-cache language is deliberately small: a cache
  # initializer followed by one or more update declarations attached to a
  # scalar block. Acceptance probabilities always come from ordinary old-state
  # and proposed-state block evaluation.
  removed_ratio_pat <- "(^|})[[:space:]]*(?:ratio|logratio)[[:space:]]*\\{"
  if (any(grepl(removed_ratio_pat, src, perl = TRUE))) {
    stop(
      "`ratio { ... }` and `logratio { ... }` are not supported. ",
      "Define the changing log-posterior terms in the block and maintain deterministic state with `cache`/`update`.",
      call. = FALSE
    )
  }

  # Let users write attached cache/update declarations on the same line as the
  # preceding block. Splitting only these hobbs keywords keeps ordinary C
  # untouched and lets the line-oriented parser continue to work.
  out <- character()
  for (line in src) {
    line <- gsub("}[[:space:]]+cache[[:space:]]+", "}\ncache ", line, perl = TRUE)
    line <- gsub("}[[:space:]]+update[[:space:]]+", "}\nupdate ", line, perl = TRUE)
    out <- c(out, strsplit(line, "\n", fixed = TRUE)[[1L]])
  }
  out
}


collect_braced_declaration <- function(src, i, keyword) {
  line <- src[[i]]
  bal <- count_char(line, "{") - count_char(line, "}")
  lines <- line
  j <- i
  while (bal > 0L && j < length(src)) {
    j <- j + 1L
    lines <- c(lines, src[[j]])
    bal <- bal + count_char(src[[j]], "{") - count_char(src[[j]], "}")
  }
  if (bal != 0L) stop("Unbalanced braces in ", keyword, " declaration.", call. = FALSE)
  list(lines = lines, next_i = j + 1L)
}

cache_body_lines <- function(lines) {
  extract_func_body_lines(lines)
}

extract_attached_cache_declarations <- function(src) {
  src <- normalize_attached_cache_syntax(src)
  out <- character()
  caches <- list()
  i <- 1L
  block_pat <- "^([[:space:]]*)block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)(?:[[:space:]]*\\([[:space:]]*([^)]*)[[:space:]]*\\))?[[:space:]]*\\{"
  cache_pat <- "^[[:space:]]*cache[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([^)]*)[[:space:]]*\\)[[:space:]]*\\{"
  update_pat <- "^[[:space:]]*update[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([^)]*)[[:space:]]*\\)[[:space:]]*\\{"
  while (i <= length(src)) {
    line <- src[[i]]
    m <- regexec(block_pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) >= 3L) {
      block_name <- hit[[3L]]
      block_index_text <- if (length(hit) >= 4L && !is.na(hit[[4L]])) trimws(hit[[4L]]) else ""
      block_index_vars <- character()
      if (nzchar(block_index_text) && block_index_text != "1") {
        block_index_vars <- split_top_level_commas(block_index_text)
      }
      block_index <- if (length(block_index_vars) == 1L) block_index_vars[[1L]] else "hobbs_block_index"
      blk <- collect_braced_declaration(src, i, "block")
      out <- c(out, blk$lines)
      i <- blk$next_i
      repeat {
        if (i > length(src)) break
        next_line <- src[[i]]
        mc <- regexec(cache_pat, next_line, perl = TRUE)
        hc <- regmatches(next_line, mc)[[1L]]
        if (length(hc) == 3L) {
          cc <- collect_braced_declaration(src, i, "cache")
          caches[[length(caches) + 1L]] <- list(
            kind = "cache",
            block = block_name,
            block_index = block_index,
            block_index_vars = block_index_vars,
            name = hc[[2L]],
            dims = trimws(hc[[3L]]),
            body = cache_body_lines(cc$lines)
          )
          i <- cc$next_i
          next
        }
        mu <- regexec(update_pat, next_line, perl = TRUE)
        hu <- regmatches(next_line, mu)[[1L]]
        if (length(hu) == 3L) {
          uu <- collect_braced_declaration(src, i, "update")
          caches[[length(caches) + 1L]] <- list(
            kind = "update",
            block = block_name,
            block_index = block_index,
            block_index_vars = block_index_vars,
            name = hu[[2L]],
            dims = trimws(hu[[3L]]),
            body = cache_body_lines(uu$lines)
          )
          i <- uu$next_i
          next
        }
        break
      }
      next
    }
    # Standalone declarations are kept as ordinary source so C compilation
    # fails clearly; cache/update declarations must be attached to a block.
    out <- c(out, line)
    i <- i + 1L
  }
  list(src = out, caches = caches)
}


cache_dims_to_c_len <- function(dims) {
  parts <- split_top_level_commas(dims)
  if (!length(parts)) stop("cache declarations need an extent, e.g. cache mu(n)", call. = FALSE)
  paste(sprintf("((size_t)(%s))", parts), collapse = " * ")
}

cache_index_expr <- function(dims) {
  parts <- split_top_level_commas(dims)
  if (length(parts) == 1L) return("((int)(i) - 1)")
  if (length(parts) == 2L) return(sprintf("((int)(i) - 1) + ((int)(j) - 1) * (%s)", parts[[1L]]))
  stop("cache declarations currently support one- or two-dimensional caches.", call. = FALSE)
}

generate_param_access_macros <- function(param_info, theta_name, prefix = "") {
  if (is.null(param_info) || !length(param_info$spec)) return(character())
  out <- character()
  for (item in param_info$spec) {
    nm <- item$name
    macro_nm <- paste0(prefix, nm)
    off <- as.integer(item$offset)
    nr <- as.integer(item$nrow)
    nc <- as.integer(item$ncol)
    is_discrete <- identical(item$value_type %||% "continuous", "discrete")
    cast0 <- if (is_discrete) "((int)" else ""
    cast1 <- if (is_discrete) ")" else ""
    if (nc == 1L) {
      out <- c(out, sprintf("#define %s(i) %s%s[%d + ((int)(i) - 1)]%s", macro_nm, cast0, theta_name, off, cast1))
    } else {
      out <- c(out, sprintf("#define %s(i, j) %s%s[%d + ((int)(i) - 1) + ((int)(j) - 1) * %d]%s", macro_nm, cast0, theta_name, off, nr, cast1))
    }
  }
  out
}

translate_current_proposal_refs <- function(lines, param_info, block_name = NULL, block_index = "hobbs_block_index", active_args = NULL) {
  if (is.null(param_info) || !length(param_info$spec) || !length(lines)) return(lines)
  if (any(grepl("\\bdelta[[:space:]]*\\(", lines, perl = TRUE))) {
    stop(
      "`delta(...)` is not part of the cache/update language. Write `proposal(x) - current(x)` explicitly.",
      call. = FALSE
    )
  }
  if (!is.null(block_name) && nzchar(block_name)) {
    # The theta pointer already contains the scalar proposal. The old accepted
    # value for the active component is passed separately, so current(beta(j))
    # is O(1) and no parameter-vector copy is required.
    if (is.null(active_args) || !length(active_args)) {
      pmap <- setNames(param_info$spec, vapply(param_info$spec, `[[`, character(1), "name"))
      active_info <- pmap[[block_name]]
      # `block sigma(1)` has no named index variable, but current/proposal
      # references still need to recognize sigma(1) as the active scalar.
      active_args <- if (!is.null(active_info) && as.integer(active_info$len) == 1L) "1" else block_index
    }
    active_args <- trimws(active_args)
    active_call <- paste(active_args, collapse = "[[:space:]]*,[[:space:]]*")
    active <- sprintf("%s[[:space:]]*\\([[:space:]]*%s[[:space:]]*\\)", block_name, active_call)
    replacement_call <- sprintf("%s(%s)", block_name, paste(active_args, collapse = ","))
    lines <- gsub(sprintf("proposal[[:space:]]*\\([[:space:]]*%s[[:space:]]*\\)", active), replacement_call, lines, perl = TRUE)
    lines <- gsub(sprintf("current[[:space:]]*\\([[:space:]]*%s[[:space:]]*\\)", active), "hobbs_current_value", lines, perl = TRUE)
  }
  for (item in param_info$spec) {
    nm <- item$name
    # Other parameters are unchanged during this scalar update, so current(x)
    # and proposal(x) both reduce to the ordinary accessor.
    pat_prop <- sprintf("proposal[[:space:]]*\\([[:space:]]*%s[[:space:]]*\\(([^()]*)\\)[[:space:]]*\\)", nm)
    lines <- gsub(pat_prop, sprintf("%s(\\1)", nm), lines, perl = TRUE)
    pat_cur <- sprintf("current[[:space:]]*\\([[:space:]]*%s[[:space:]]*\\(([^()]*)\\)[[:space:]]*\\)", nm)
    lines <- gsub(pat_cur, sprintf("%s(\\1)", nm), lines, perl = TRUE)
  }
  lines
}


cache_direct_additive_write_pattern <- function(cache_name) {
  # Deliberately line-oriented and anchored.  Bodies that use compound
  # statements, multiline assignments, pointer aliases, or helper calls fall
  # back to the ordinary backup-and-restore path rather than being guessed reversible.
  paste0(
    "^[[:space:]]*", cache_name,
    "[[:space:]]*\\(.*\\)[[:space:]]*(\\+=|-=)[[:space:]]*(.*;[[:space:]]*)$"
  )
}

cache_reference_pattern <- function(cache_name) {
  # Match the generated cache accessor and its private backing pointer. Update
  # bodies that reach storage through an alias are conservatively treated as
  # non-reversible and use an internal backup on rejection.
  paste0(
    "(?:\\b", cache_name, "[[:space:]]*\\(",
    "|\\bhobbs_cache_", cache_name, "\\b)"
  )
}


cache_update_is_reversible <- function(update_item) {
  # A cache update is algebraically reversible only when every occurrence of
  # the attached cache is the left-hand side of a direct += or -= statement,
  # and the right-hand side does not read that cache.  Rejection then executes
  # the same body with each additive operator inverted while theta still holds
  # the proposal.  This preserves branch decisions and right-hand sides exactly.
  nm <- update_item$name
  lines <- trimws(update_item$body)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(FALSE)
  write_pattern <- cache_direct_additive_write_pattern(nm)
  reference_pattern <- cache_reference_pattern(nm)
  saw_write <- FALSE

  for (ln in lines) {
    match <- regexec(write_pattern, ln, perl = TRUE)
    hit <- regmatches(ln, match)[[1L]]
    if (length(hit) == 3L) {
      saw_write <- TRUE
      rhs <- hit[[3L]]
      if (grepl(reference_pattern, rhs, perl = TRUE)) return(FALSE)
    } else if (grepl(reference_pattern, ln, perl = TRUE)) {
      return(FALSE)
    }
  }
  saw_write
}

invert_additive_cache_update_body <- function(lines, cache_name) {
  write_pattern <- cache_direct_additive_write_pattern(cache_name)
  vapply(lines, function(line) {
    match <- regexec(write_pattern, line, perl = TRUE)
    hit <- regmatches(line, match)[[1L]]
    if (length(hit) != 3L) return(line)
    operator <- hit[[2L]]
    replacement <- if (identical(operator, "+=")) "-=" else "+="
    # Only the first compound operator can be the anchored cache write.  Any
    # further operators belong to the right-hand side and must remain intact.
    sub(if (identical(operator, "+=")) "\\+=" else "-=", replacement, line, perl = TRUE)
  }, character(1), USE.NAMES = FALSE)
}


cache_multi_index_declarations <- function(block_name, block_index_vars, param_info) {
  if (length(block_index_vars) <= 1L) return(character())
  if (length(block_index_vars) != 2L || any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", block_index_vars, perl = TRUE))) {
    stop("Multi-index cache update syntax currently supports exactly two index variables, e.g. `block u(j, l) { ... }`.", call. = FALSE)
  }
  pmap <- setNames(param_info$spec, vapply(param_info$spec, `[[`, character(1), "name"))
  info <- pmap[[block_name]]
  if (is.null(info) || as.integer(info$ncol) <= 1L) {
    stop("`block ", block_name, "(", paste(block_index_vars, collapse = ", "), ")` requires a two-dimensional `param ", block_name, "(nrow, ncol)` declaration.", call. = FALSE)
  }
  nr <- as.integer(info$nrow)
  c(
    sprintf("  int %s = ((hobbs_block_index - 1) %% %d) + 1;", block_index_vars[[1L]], nr),
    sprintf("  int %s = ((hobbs_block_index - 1) / %d) + 1;", block_index_vars[[2L]], nr)
  )
}

cache_block_update_is_reversible <- function(update_items, all_cache_names) {
  if (!length(update_items) || !all(vapply(update_items, cache_update_is_reversible, logical(1)))) {
    return(FALSE)
  }
  # No right-hand side or control expression may read any mutable cache.  Such
  # dependencies can make an operator-wise inverse order-dependent.  Strip the
  # recognized direct LHS before checking, and use the general backup path for
  # every ambiguous body.
  for (item in update_items) {
    own_pattern <- cache_direct_additive_write_pattern(item$name)
    residual <- vapply(item$body, function(line) {
      match <- regexec(own_pattern, trimws(line), perl = TRUE)
      hit <- regmatches(trimws(line), match)[[1L]]
      if (length(hit) == 3L) hit[[3L]] else line
    }, character(1), USE.NAMES = FALSE)
    body <- paste(residual, collapse = "\n")
    for (nm in all_cache_names) {
      if (grepl(cache_reference_pattern(nm), body, perl = TRUE)) {
        return(FALSE)
      }
    }
  }
  TRUE
}


generate_cache_c <- function(cache_decls, param_info, theta_name) {
  if (!length(cache_decls)) return(character())
  cache_items <- cache_decls[vapply(cache_decls, function(x) identical(x$kind, "cache"), logical(1))]
  update_items <- cache_decls[vapply(cache_decls, function(x) identical(x$kind, "update"), logical(1))]
  if (!length(cache_items)) {
    stop("cache update syntax requires at least one attached cache initializer.", call. = FALSE)
  }
  cache_names <- vapply(cache_items, `[[`, character(1), "name")
  if (anyDuplicated(cache_names)) {
    stop(
      "Each cache may be declared once; duplicate cache declaration(s): ",
      paste(unique(cache_names[duplicated(cache_names)]), collapse = ", "),
      call. = FALSE
    )
  }
  cache_map <- setNames(cache_items, cache_names)
  for (item in update_items) {
    if (!(item$name %in% names(cache_map))) {
      stop(
        "update for unknown cache `", item$name, "`. Attach a cache ", item$name,
        "(...) initializer first.",
        call. = FALSE
      )
    }
    if (!identical(trimws(item$dims), trimws(cache_map[[item$name]]$dims))) {
      stop(
        "update dimension for cache `", item$name,
        "` must match its cache declaration: update ", item$name, "(", item$dims,
        ") vs cache ", item$name, "(", cache_map[[item$name]]$dims, ").",
        call. = FALSE
      )
    }
  }

  out <- c(
    "",
    "/* generated by hobbs from attached cache/update declarations */",
    "#include <stdlib.h>",
    "#include <string.h>",
    ""
  )
  for (cache in cache_items) {
    nm <- cache$name
    index_expr <- cache_index_expr(cache$dims)
    out <- c(
      out,
      sprintf("static double *hobbs_cache_%s = NULL;", nm),
      sprintf("static double *hobbs_cache_%s_saved = NULL;", nm),
      sprintf("static size_t hobbs_cache_%s_len = 0;", nm),
      if (length(split_top_level_commas(cache$dims)) == 1L) {
        sprintf("#define %s(i) hobbs_cache_%s[%s]", nm, nm, index_expr)
      } else {
        sprintf("#define %s(i, j) hobbs_cache_%s[%s]", nm, nm, index_expr)
      },
      ""
    )
  }

  # Backup storage is allocated only if a non-reversible update or a legacy
  # compatibility path needs it. Ordinary additive cache updates keep only the
  # live aligned array and are reversed algebraically after rejection.
  for (cache in cache_items) {
    nm <- cache$name
    out <- c(
      out,
      sprintf("static inline int hobbs_cache_ensure_saved_%s(void) {", nm),
      sprintf("  if (hobbs_cache_%s_saved != NULL) return 1;", nm),
      sprintf("  hobbs_cache_%s_saved = (double*)hobbs_aligned_malloc(hobbs_cache_%s_len * sizeof(double));", nm, nm),
      sprintf("  return hobbs_cache_%s_saved != NULL;", nm),
      "}",
      ""
    )
  }

  out <- c(
    out,
    "void hobbs_cache_free(void);",
    "",
    sprintf("int hobbs_cache_init(const double *%s, int dim) {", theta_name),
    "  (void)dim;"
  )
  for (cache in cache_items) {
    nm <- cache$name
    out <- c(
      out,
      sprintf("  hobbs_cache_%s_len = %s;", nm, cache_dims_to_c_len(cache$dims)),
      # Return directly on allocation failure rather than jumping to a label
      # after the user cache body. A user cache initializer may declare a VLA
      # (for example, `int active[p];`), and C forbids a goto from entering the
      # scope of a variably modified object. Direct cleanup-and-return is valid
      # regardless of declarations in the translated user body.
      sprintf("  if (hobbs_cache_%s_len == 0) { hobbs_cache_free(); return 1; }", nm),
      sprintf("  hobbs_cache_%s = (double*)hobbs_aligned_malloc(hobbs_cache_%s_len * sizeof(double));", nm, nm),
      sprintf("  if (hobbs_cache_%s == NULL) { hobbs_cache_free(); return 1; }", nm),
      sprintf("  memset(hobbs_cache_%s, 0, hobbs_cache_%s_len * sizeof(double));", nm, nm)
    )
  }
  for (cache in cache_items) {
    body <- translate_r_like_for_loops(cache$body)
    out <- c(out, paste0("  ", body))
  }
  out <- c(
    out,
    "  return 0;",
    "}",
    "",
    "void hobbs_cache_free(void) {"
  )
  for (cache in cache_items) {
    nm <- cache$name
    out <- c(
      out,
      sprintf("  hobbs_aligned_free(hobbs_cache_%s); hobbs_cache_%s = NULL;", nm, nm),
      sprintf("  hobbs_aligned_free(hobbs_cache_%s_saved); hobbs_cache_%s_saved = NULL;", nm, nm),
      sprintf("  hobbs_cache_%s_len = 0;", nm)
    )
  }
  out <- c(out, "}", "")

  # Compatibility snapshot ABI for hand-written libraries and fallback sampler
  # paths. Generated scalar kernels below save only the caches touched by the
  # active block.
  out <- c(out, "void hobbs_cache_snapshot(void) {")
  for (cache in cache_items) {
    nm <- cache$name
    out <- c(
      out,
      sprintf("  if (!hobbs_cache_ensure_saved_%s()) abort();", nm),
      sprintf("  memcpy(hobbs_cache_%s_saved, hobbs_cache_%s, hobbs_cache_%s_len * sizeof(double));", nm, nm, nm)
    )
  }
  out <- c(out, "}", "", "void hobbs_cache_restore(void) {")
  for (cache in cache_items) {
    nm <- cache$name
    out <- c(
      out,
      sprintf("  if (hobbs_cache_%s_saved != NULL) memcpy(hobbs_cache_%s, hobbs_cache_%s_saved, hobbs_cache_%s_len * sizeof(double));", nm, nm, nm, nm)
    )
  }
  out <- c(out, "}", "")

  blocks <- unique(vapply(update_items, `[[`, character(1), "block"))
  for (block_name in blocks) {
    updates <- update_items[vapply(update_items, function(x) identical(x$block, block_name), logical(1))]
    index_vars <- updates[[1L]]$block_index_vars %||% character()
    index_name <- if (length(index_vars) == 1L) {
      updates[[1L]]$block_index %||% index_vars[[1L]]
    } else {
      "hobbs_block_index"
    }
    touched <- unique(vapply(updates, `[[`, character(1), "name"))
    reversible <- cache_block_update_is_reversible(updates, cache_names)

    translated_updates <- vector("list", length(updates))
    out <- c(
      out,
      sprintf("void hobbs_cache_update_%s(const double *%s, int %s, double hobbs_current_value) {", block_name, theta_name, index_name),
      sprintf("  (void)%s; (void)%s; (void)hobbs_current_value;", theta_name, index_name)
    )
    out <- c(out, cache_multi_index_declarations(block_name, index_vars, param_info))
    for (update_index in seq_along(updates)) {
      item <- updates[[update_index]]
      body <- translate_current_proposal_refs(
        item$body,
        param_info,
        block_name = block_name,
        block_index = index_name,
        active_args = index_vars
      )
      body <- translate_r_like_for_loops(body)
      translated_updates[[update_index]] <- list(name = item$name, body = body)
      out <- c(out, paste0("  ", body))
    }
    out <- c(
      out,
      "}",
      "",
      sprintf("int hobbs_cache_update_reversible_%s(void) { return %d; }", block_name, as.integer(reversible)),
      ""
    )

    if (isTRUE(reversible)) {
      out <- c(
        out,
        sprintf("hobbs_EXPORT void hobbs_cache_undo_%s(const double *%s, int %s, double hobbs_current_value) {", block_name, theta_name, index_name),
        sprintf("  (void)%s; (void)%s; (void)hobbs_current_value;", theta_name, index_name),
        cache_multi_index_declarations(block_name, index_vars, param_info)
      )
      for (translated in translated_updates) {
        inverse_body <- invert_additive_cache_update_body(translated$body, translated$name)
        out <- c(out, paste0("  ", inverse_body))
      }
      out <- c(out, "}", "")
    }

    if (isTRUE(reversible)) {
      out <- c(
        out,
        sprintf("static inline void hobbs_cache_begin_%s(const double *theta, int index, double current_value) {", block_name),
        sprintf("  hobbs_cache_update_%s(theta, index, current_value);", block_name),
        "}",
        sprintf("static inline void hobbs_cache_commit_%s(void) { }", block_name),
        sprintf("static inline void hobbs_cache_abort_%s(const double *theta, int index, double current_value) {", block_name),
        sprintf("  hobbs_cache_undo_%s(theta, index, current_value);", block_name),
        "}",
        ""
      )
    } else {
      out <- c(out, sprintf("static inline void hobbs_cache_begin_%s(const double *theta, int index, double current_value) {", block_name))
      for (nm in touched) {
        out <- c(
          out,
          sprintf("  if (!hobbs_cache_ensure_saved_%s()) abort();", nm),
          sprintf("  memcpy(hobbs_cache_%s_saved, hobbs_cache_%s, hobbs_cache_%s_len * sizeof(double));", nm, nm, nm)
        )
      }
      out <- c(
        out,
        sprintf("  hobbs_cache_update_%s(theta, index, current_value);", block_name),
        "}",
        sprintf("static inline void hobbs_cache_commit_%s(void) { }", block_name),
        sprintf("static inline void hobbs_cache_abort_%s(const double *theta, int index, double current_value) {", block_name),
        "  (void)theta; (void)index; (void)current_value;"
      )
      for (nm in touched) {
        out <- c(
          out,
          sprintf("  memcpy(hobbs_cache_%s, hobbs_cache_%s_saved, hobbs_cache_%s_len * sizeof(double));", nm, nm, nm)
        )
      }
      out <- c(out, "}", "")
    }
  }
  out
}


generate_scalar_kernels_c <- function(param_info, block_info, cache_decls) {
  if (is.null(param_info) || !length(param_info$spec) || !length(block_info)) return(character())
  block_names <- vapply(block_info, `[[`, character(1), "name")
  block_map <- setNames(block_info, block_names)
  cache_updates <- cache_decls[vapply(cache_decls, function(x) identical(x$kind, "update"), logical(1))]
  cache_blocks <- unique(vapply(cache_updates, `[[`, character(1), "block"))
  out <- c("", "/* generated scalar transaction and whole-sweep kernels */")

  for (parameter in param_info$spec) {
    name <- parameter$name
    if (is.null(block_map[[name]])) next
    offset <- as.integer(parameter$offset)
    len <- as.integer(parameter$len)
    value_type <- parameter$value_type %||% "continuous"
    has_cache <- name %in% cache_blocks

    out <- c(
      out,
      sprintf("static inline double hobbs_candidate_impl_%s(double *theta, int index, int position, double current_value, double proposed_value) {", name),
      "  (void)current_value;",
      "  theta[position] = proposed_value;"
    )
    if (has_cache) {
      out <- c(out, sprintf("  hobbs_cache_begin_%s(theta, index, current_value);", name))
    }
    out <- c(
      out,
      sprintf("  return hobbs_block_%s(theta, index);", name),
      "}",
      "",
      sprintf("hobbs_EXPORT double hobbs_scalar_candidate_%s(double *theta, int index, int position, double current_value, double proposed_value) {", name),
      sprintf("  return hobbs_candidate_impl_%s(theta, index, position, current_value, proposed_value);", name),
      "}",
      sprintf("hobbs_EXPORT void hobbs_scalar_accept_%s(void) {", name)
    )
    if (has_cache) out <- c(out, sprintf("  hobbs_cache_commit_%s();", name))
    out <- c(
      out,
      "}",
      sprintf("hobbs_EXPORT void hobbs_scalar_reject_%s(double *theta, int index, int position, double current_value, double proposed_value) {", name),
      "  (void)index; (void)proposed_value;"
    )
    if (has_cache) out <- c(out, sprintf("  hobbs_cache_abort_%s(theta, index, current_value);", name))
    out <- c(out, "  theta[position] = current_value;", "}", "")

    if (!identical(value_type, "continuous")) next

    # One scalar transition helper is shared by the compatibility sweep and
    # the fused adaptation sweep. Every coordinate still receives an ordinary
    # old-state/proposed-state Metropolis decision in sequence.
    out <- c(
      out,
      sprintf("static hobbs_ALWAYS_INLINE double hobbs_scalar_step_%s(", name),
      "    double * hobbs_RESTRICT theta,",
      "    int index,",
      "    int position,",
      "    double scale,",
      "    double normal,",
      "    double uniform,",
      "    unsigned char *accepted_out,",
      "    int *bad_index) {",
      "  const double current_value = theta[position];",
      "  const double proposed_value = current_value + scale * normal;",
      sprintf("  const double current_local = hobbs_block_%s(theta, index);", name),
      "  if (!isfinite(current_local)) {",
      "    if (bad_index != NULL) *bad_index = index;",
      "    *accepted_out = 0u;",
      "    return NAN;",
      "  }",
      sprintf("  const double proposed_local = hobbs_candidate_impl_%s(theta, index, position, current_value, proposed_value);", name),
      "  const double log_alpha = proposed_local - current_local;",
      "  const int accepted = isfinite(proposed_local) && (log_alpha >= 0.0 || uniform < exp(log_alpha));",
      "  *accepted_out = (unsigned char)accepted;",
      "  if (accepted) {"
    )
    if (has_cache) out <- c(out, sprintf("    hobbs_cache_commit_%s();", name))
    out <- c(
      out,
      "    return log_alpha;",
      "  }"
    )
    if (has_cache) out <- c(out, sprintf("  hobbs_cache_abort_%s(theta, index, current_value);", name))
    out <- c(
      out,
      "  theta[position] = current_value;",
      "  return 0.0;",
      "}",
      ""
    )

    # Compatibility ABI retained for hand-written callers and regression
    # tests. Package-generated Rust samplers use the fused sweeps below.
    out <- c(
      out,
      sprintf("hobbs_EXPORT double hobbs_sweep_%s(", name),
      "    double * hobbs_RESTRICT theta,",
      "    const double * hobbs_RESTRICT scales,",
      "    const double * hobbs_RESTRICT normals,",
      "    const double * hobbs_RESTRICT uniforms,",
      "    unsigned char * hobbs_RESTRICT accepted_flags,",
      "    int *bad_index) {",
      "  double delta_sum = 0.0;",
      "  if (bad_index != NULL) *bad_index = 0;",
      sprintf("  for (int local = 0; local < %d; ++local) {", len),
      "    const int index = local + 1;",
      sprintf("    const int position = %d + local;", offset),
      sprintf("    const double delta = hobbs_scalar_step_%s(theta, index, position, scales[local], normals[local], uniforms[local], &accepted_flags[local], bad_index);", name),
      "    if (isnan(delta)) return NAN;",
      "    delta_sum += delta;",
      "  }",
      "  return delta_sum;",
      "}",
      ""
    )

    # Frozen-scaling production sweep. Once scalar adaptation is over, the C ABI
    # no longer needs tuning factors, warmup counters, or adaptation bounds.
    # the C ABI. Acceptance accounting stays in C so Rust does not need a
    # second per-coordinate pass over flags.
    out <- c(
      out,
      sprintf("hobbs_EXPORT double hobbs_sweep_sample_%s(", name),
      "    double * hobbs_RESTRICT theta,",
      "    const double * hobbs_RESTRICT scales,",
      "    const double * hobbs_RESTRICT normals,",
      "    const double * hobbs_RESTRICT uniforms,",
      "    uint64_t * hobbs_RESTRICT accept_counts,",
      "    uint64_t *accepted_count,",
      "    int *bad_index) {",
      "  double delta_sum = 0.0;",
      "  uint64_t accepted_total = 0u;",
      "  if (accepted_count != NULL) *accepted_count = 0u;",
      "  if (bad_index != NULL) *bad_index = 0;",
      sprintf("  for (int local = 0; local < %d; ++local) {", len),
      "    const int index = local + 1;",
      sprintf("    const int position = %d + local;", offset),
      "    unsigned char accepted = 0u;",
      sprintf("    const double delta = hobbs_scalar_step_%s(theta, index, position, scales[local], normals[local], uniforms[local], &accepted, bad_index);", name),
      "    if (isnan(delta)) return NAN;",
      "    delta_sum += delta;",
      "    if (accepted != 0u) {",
      "      accept_counts[local] += 1u;",
      "      accepted_total += 1u;",
      "    }",
      "  }",
      "  if (accepted_count != NULL) *accepted_count = accepted_total;",
      "  return delta_sum;",
      "}",
      ""
    )

    out <- c(
      out,
      sprintf("hobbs_EXPORT double hobbs_sweep_adapt_%s(", name),
      "    double * hobbs_RESTRICT theta,",
      "    double * hobbs_RESTRICT scales,",
      "    double * hobbs_RESTRICT tuning_factors,",
      "    const double * hobbs_RESTRICT normals,",
      "    const double * hobbs_RESTRICT uniforms,",
      "    uint64_t * hobbs_RESTRICT accept_counts,",
      "    uint64_t * hobbs_RESTRICT warmup_accept_counts,",
      "    int adapting,",
      "    double initial_step,",
      "    double scale_up,",
      "    double scale_down,",
      "    double min_tuning,",
      "    double max_tuning,",
      "    double min_scale,",
      "    double max_scale,",
      "    uint64_t *accepted_count,",
      "    int *bad_index) {",
      "  double delta_sum = 0.0;",
      "  uint64_t accepted_total = 0u;",
      "  if (accepted_count != NULL) *accepted_count = 0u;",
      "  if (bad_index != NULL) *bad_index = 0;",
      sprintf("  for (int local = 0; local < %d; ++local) {", len),
      "    const int index = local + 1;",
      sprintf("    const int position = %d + local;", offset),
      "    unsigned char accepted = 0u;",
      sprintf("    const double delta = hobbs_scalar_step_%s(theta, index, position, scales[local], normals[local], uniforms[local], &accepted, bad_index);", name),
      "    if (isnan(delta)) return NAN;",
      "    delta_sum += delta;",
      "    if (accepted != 0u) {",
      "      accept_counts[local] += 1u;",
      "      accepted_total += 1u;",
      "      if (adapting) warmup_accept_counts[local] += 1u;",
      "    }",
      "    if (adapting) {",
      "      double tuning = tuning_factors[local] * (accepted != 0u ? scale_up : scale_down);",
      "      if (tuning < min_tuning) tuning = min_tuning;",
      "      else if (tuning > max_tuning) tuning = max_tuning;",
      "      tuning_factors[local] = tuning;",
      "      double proposal_scale = initial_step * tuning;",
      "      if (proposal_scale < min_scale) proposal_scale = min_scale;",
      "      else if (proposal_scale > max_scale) proposal_scale = max_scale;",
      "      scales[local] = proposal_scale;",
      "    }",
      "  }",
      "  if (accepted_count != NULL) *accepted_count = accepted_total;",
      "  return delta_sum;",
      "}",
      ""
    )
  }
  out
}


expand_multi_block_declarations <- function(src) {
  src <- normalize_attached_cache_syntax(src)
  out <- character()
  i <- 1L
  pat <- "^([[:space:]]*)block[[:space:]]+(.+?)[[:space:]]*\\{(.*)$"
  while (i <= length(src)) {
    line <- src[[i]]
    m <- regexec(pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 4L) {
      indent <- hit[[2L]]
      head <- trimws(hit[[3L]])
      specs <- split_top_level_commas(head)
      if (length(specs) > 1L) {
        block_lines <- line
        bal <- count_char(line, "{") - count_char(line, "}")
        j <- i
        while (bal > 0L && j < length(src)) {
          j <- j + 1L
          block_lines <- c(block_lines, src[[j]])
          bal <- bal + count_char(src[[j]], "{") - count_char(src[[j]], "}")
        }
        for (spec in specs) {
          copy <- block_lines
          copy[[1L]] <- sub(pat, paste0(indent, "block ", spec, " {\\3"), copy[[1L]], perl = TRUE)
          out <- c(out, copy)
        }
        i <- j + 1L
        next
      }
    }
    out <- c(out, line)
    i <- i + 1L
  }
  out
}

add_implicit_block_target <- function(src) {
  out <- character()
  i <- 1L
  pat <- "^([[:space:]]*)block[[:space:]]+[A-Za-z_][A-Za-z0-9_]*(?:[[:space:]]*\\([[:space:]]*(?:[A-Za-z_][A-Za-z0-9_]*|[0-9]+)(?:[[:space:]]*,[[:space:]]*(?:[A-Za-z_][A-Za-z0-9_]*|[0-9]+))*[[:space:]]*\\))?[[:space:]]*\\{(.*)$"
  while (i <= length(src)) {
    line <- src[[i]]
    m <- regexec(pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 3L) {
      block_lines <- line
      bal <- count_char(line, "{") - count_char(line, "}")
      j <- i
      while (bal > 0L && j < length(src)) {
        j <- j + 1L
        block_lines <- c(block_lines, src[[j]])
        bal <- bal + count_char(src[[j]], "{") - count_char(src[[j]], "}")
      }
      # Normalize a simple one-line declaration such as
      #   block beta(j) { beta(j) ~ dnorm(0, 1); }
      # before inserting the implicit target accumulator.  The previous code
      # treated the entire line as a header, which placed `target` and `return`
      # outside the generated C function.
      if (length(block_lines) == 1L && bal == 0L) {
        open_at <- regexpr("{", block_lines[[1L]], fixed = TRUE)[[1L]]
        close_at <- gregexpr("}", block_lines[[1L]], fixed = TRUE)[[1L]]
        close_at <- close_at[close_at > open_at]
        if (open_at > 0L && length(close_at)) {
          close_at <- close_at[[length(close_at)]]
          original <- block_lines[[1L]]
          header <- substr(original, 1L, open_at)
          body <- trimws(substr(original, open_at + 1L, close_at - 1L))
          suffix <- substr(original, close_at + 1L, nchar(original))
          header_indent <- sub("^([[:space:]]*).*", "\\1", original, perl = TRUE)
          block_lines <- c(
            header,
            if (nzchar(body)) paste0(header_indent, "  ", body),
            paste0(header_indent, "}", suffix)
          )
        }
      }
      body_txt <- paste(block_lines[-1L], collapse = "\n")
      returns <- gregexpr("\\breturn[[:space:]]+([^;]+);", body_txt, perl = TRUE)[[1L]]
      has_non_boundary_return <- FALSE
      if (!identical(returns[[1L]], -1L)) {
        ret_txt <- regmatches(body_txt, gregexpr("\\breturn[[:space:]]+([^;]+);", body_txt, perl = TRUE))[[1L]]
        exprs <- trimws(sub("^return[[:space:]]+", "", sub(";$", "", ret_txt)))
        # Treat common impossible-state guards as boundary returns; still add final return target.
        boundary <- exprs %in% c("-INFINITY", "-Inf", "-HUGE_VAL")
        has_non_boundary_return <- any(!boundary)
      }
      if (!has_non_boundary_return) {
        has_target_decl <- grepl("\\bdouble[[:space:]]+target[[:space:]]*=", paste(block_lines, collapse = "\n"), perl = TRUE)
        header_indent <- sub("^([[:space:]]*).*", "\\1", block_lines[[1L]], perl = TRUE)
        body_indent <- paste0(header_indent, "  ")
        # Insert target declaration immediately after the block header unless user already declared it.
        if (!has_target_decl) {
          block_lines <- append(block_lines, paste0(body_indent, "double target = 0.0;"), after = 1L)
        }
        # Insert final return immediately before the closing brace line.
        if (length(block_lines) >= 2L) {
          block_lines <- append(block_lines, paste0(body_indent, "return target;"), after = length(block_lines) - 1L)
        }
      }
      out <- c(out, block_lines)
      i <- j + 1L
      next
    }
    out <- c(out, line)
    i <- i + 1L
  }
  out
}


parse_block_declarations <- function(model_c) {
  src <- readLines(model_c, warn = FALSE)
  src <- normalize_attached_cache_syntax(src)
  src <- expand_multi_block_declarations(src)
  # Supports scalar/indexed blocks.  Multi-index syntax still denotes one
  # scalar parameter coordinate after flattening; it is not a multivariate
  # proposal:
  #   block u0(p) { ... }
  # scalar one-parameter blocks:
  #   block sigma(1) { ... }
  # Bare `block beta { ... }` is parsed only so resolve_block_runtime() can
  # reject it with a focused scalar-only diagnostic.
  pat_multi <- "^[[:space:]]*block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([^)]*,[^)]*)[[:space:]]*\\)[[:space:]]*\\{"
  pat_index <- "^[[:space:]]*block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\)[[:space:]]*\\{"
  pat_one <- "^[[:space:]]*block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*1[[:space:]]*\\)[[:space:]]*\\{"
  pat_group <- "^[[:space:]]*block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\{"
  out <- list()
  for (line in src) {
    m <- regexec(pat_multi, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 3L) {
      idx <- split_top_level_commas(hit[[3L]])
      out[[length(out) + 1L]] <- list(name = hit[[2L]], index = paste(idx, collapse = ","), type = "indexed", scalar_one_syntax = FALSE, n_index = length(idx))
      next
    }
    m <- regexec(pat_index, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 3L) {
      out[[length(out) + 1L]] <- list(name = hit[[2L]], index = hit[[3L]], type = "indexed", scalar_one_syntax = FALSE)
      next
    }
    m <- regexec(pat_one, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 2L) {
      out[[length(out) + 1L]] <- list(name = hit[[2L]], index = "1", type = "indexed", scalar_one_syntax = TRUE)
      next
    }
    m <- regexec(pat_group, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 2L) {
      out[[length(out) + 1L]] <- list(name = hit[[2L]], index = NA_character_, type = "group", scalar_one_syntax = FALSE)
    }
  }
  if (length(out)) {
    nms <- vapply(out, `[[`, character(1), "name")
    if (anyDuplicated(nms)) stop("Block function names must be unique; use exactly one scalar/indexed declaration such as `block name(i)` or `block name(1)` for each parameter.", call. = FALSE)
  }
  out
}


resolve_block_runtime <- function(update, param_info, block_info) {
  if (!identical(update, "block")) return(list())
  if (is.null(param_info) || !length(param_info$spec)) {
    stop("`update = \"block\"` requires `param ...` or `dparam ...` declarations.", call. = FALSE)
  }
  if (is.null(block_info) || !length(block_info)) {
    stop("`update = \"block\"` requires at least one scalar block such as `block name(i) { ... }`.", call. = FALSE)
  }
  block_names <- vapply(block_info, `[[`, character(1), "name")
  specs <- param_info$spec
  param_names <- vapply(specs, `[[`, character(1), "name")
  extra <- setdiff(block_names, param_names)
  if (length(extra)) {
    stop("Block function(s) do not match any declared parameter: ", paste(extra, collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(param_names, block_names)
  if (length(missing)) {
    stop("`update = \"block\"` needs a block function for each parameter block. Missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  block_type <- setNames(vapply(block_info, function(x) x$type %||% "indexed", character(1)), block_names)
  grouped <- param_names[block_type[param_names] != "indexed"]
  if (length(grouped)) {
    stop(
      "hobbs block mode is scalar Metropolis-within-Gibbs only. Replace bare grouped declaration(s) ",
      paste(sprintf("`block %s`", grouped), collapse = ", "),
      " with indexed scalar declaration(s), e.g. ",
      paste(sprintf("`block %s(j)`", grouped), collapse = ", "),
      ". Every coordinate is then visited once per sweep; no multivariate block proposal is made.",
      call. = FALSE
    )
  }

  block_map <- setNames(block_info, block_names)
  for (parameter in specs) {
    declaration <- block_map[[parameter$name]]
    nrow <- as.integer(parameter$nrow)
    ncol <- as.integer(parameter$ncol)
    len <- as.integer(parameter$len)
    n_index <- as.integer(declaration$n_index %||% 1L)

    if (isTRUE(declaration$scalar_one_syntax) && len != 1L) {
      stop(
        "`block ", parameter$name, "(1)` is only valid for a singleton declaration. ",
        "Use `block ", parameter$name, "(j)` so all ", len,
        " scalar coordinates are evaluated with their active index.",
        call. = FALSE
      )
    }
    if (ncol > 1L && n_index != 2L) {
      stop(
        "Two-dimensional parameter `", parameter$name, "(", nrow, ", ", ncol,
        ")` requires a two-index scalar declaration such as `block ",
        parameter$name, "(j, l)`. hobbs flattens it internally but still updates ",
        "exactly one matrix element at a time.",
        call. = FALSE
      )
    }
    if (ncol == 1L && n_index != 1L) {
      stop(
        "Vector parameter `", parameter$name, "(", nrow,
        ")` requires one scalar index, e.g. `block ", parameter$name, "(j)`.",
        call. = FALSE
      )
    }
  }

  out <- lapply(specs, function(x) list(
    name = x$name,
    offset = as.integer(x$offset),
    len = as.integer(x$len),
    type = "indexed",
    value_type = x$value_type %||% "continuous",
    lower = as.integer(x$lower %||% 0L),
    upper = as.integer(x$upper %||% 0L)
  ))
  # Discrete blocks are safe and often necessary to initialize first.  For
  # example, a ZIP model starts z at 0 but y>0 requires z=1 before continuous
  # parameter blocks have finite likelihood.
  ord <- order(vapply(out, function(x) if (identical(x$value_type, "discrete")) 0L else 1L, integer(1)))
  out[ord]
}


parse_param_declarations <- function(model_c, data = NULL) {
  src <- readLines(model_c, warn = FALSE)
  specs <- list()
  offset <- 0L
  names_out <- character()

  # Continuous parameters:
  #   param beta(2)
  #   param theta(n_row, n_col) save=mean
  # Discrete integer parameters:
  #   dparam z(n, 0, 1)
  #   dparam z(n, 0, 1) save=mean
  # where the final two entries are inclusive integer bounds.  Parameters use
  # full-chain output unless the declaration has the exact `save=mean` suffix.
  save_suffix <- "(?:[[:space:]]+save[[:space:]]*=[[:space:]]*(mean))?"
  pat_param <- paste0(
    "^[[:space:]]*param[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\(",
    "[[:space:]]*([A-Za-z_][A-Za-z0-9_]*|[0-9]+)[[:space:]]*",
    "(?:,[[:space:]]*([A-Za-z_][A-Za-z0-9_]*|[0-9]+)[[:space:]]*)?\\)",
    save_suffix, "[[:space:]]*;?[[:space:]]*(?://.*)?$"
  )
  pat_dparam <- paste0(
    "^[[:space:]]*dparam[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\(",
    "[[:space:]]*([A-Za-z_][A-Za-z0-9_]*|[0-9]+)[[:space:]]*,",
    "[[:space:]]*([-]?[A-Za-z_][A-Za-z0-9_]*|[-]?[0-9]+)[[:space:]]*,",
    "[[:space:]]*([-]?[A-Za-z_][A-Za-z0-9_]*|[-]?[0-9]+)[[:space:]]*\\)",
    save_suffix, "[[:space:]]*;?[[:space:]]*(?://.*)?$"
  )

  add_spec <- function(nm, nr, nc, value_type = "continuous", lower = NA_integer_,
                       upper = NA_integer_, save_mode = "chain", line = "") {
    if (is.na(nr) || is.na(nc) || nr <= 0L || nc <= 0L) {
      stop("Invalid parameter declaration: ", trimws(line), call. = FALSE)
    }
    if (nm %in% c("dim", "for", "if", "while", "switch", "return", "sizeof", "param", "dparam", "block")) {
      stop("Invalid parameter block name `", nm, "` in parameter declaration.", call. = FALSE)
    }
    len_dbl <- as.numeric(nr) * as.numeric(nc)
    if (!is.finite(len_dbl) || len_dbl > .Machine$integer.max) {
      stop("Parameter declaration is too large: ", nm, call. = FALSE)
    }
    len <- as.integer(len_dbl)
    start <- offset
    item_names <- if (nc == 1L) {
      sprintf("%s[%d]", nm, seq_len(nr))
    } else {
      unlist(lapply(seq_len(nc), function(j) {
        sprintf("%s[%d,%d]", nm, seq_len(nr), j)
      }), use.names = FALSE)
    }
    specs[[length(specs) + 1L]] <<- list(
      name = nm, nrow = as.integer(nr), ncol = as.integer(nc),
      len = len, offset = start, value_type = value_type,
      lower = if (is.na(lower)) NA_integer_ else as.integer(lower),
      upper = if (is.na(upper)) NA_integer_ else as.integer(upper),
      save = save_mode, names = item_names
    )
    names_out <<- c(names_out, item_names)
    offset <<- offset + len
  }

  for (line in src) {
    md <- regexec(pat_dparam, line, perl = TRUE)
    hd <- regmatches(line, md)[[1L]]
    if (length(hd) == 6L) {
      nm <- hd[[2L]]
      nr <- resolve_param_extent(hd[[3L]], data = data, context = trimws(line))
      lo <- resolve_integer_extent(hd[[4L]], data = data, context = trimws(line))
      hi <- resolve_integer_extent(hd[[5L]], data = data, context = trimws(line))
      save_mode <- if (nzchar(hd[[6L]])) hd[[6L]] else "chain"
      if (hi < lo) stop("dparam upper bound must be >= lower bound in: ", trimws(line), call. = FALSE)
      add_spec(nm, nr, 1L, value_type = "discrete", lower = lo, upper = hi,
               save_mode = save_mode, line = line)
      next
    }

    m <- regexec(pat_param, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 5L) {
      nm <- hit[[2L]]
      nr_expr <- hit[[3L]]
      nc_expr <- hit[[4L]]
      save_mode <- if (nzchar(hit[[5L]])) hit[[5L]] else "chain"
      nr <- resolve_param_extent(nr_expr, data = data, context = trimws(line))
      nc <- if (nzchar(nc_expr)) resolve_param_extent(nc_expr, data = data, context = trimws(line)) else 1L
      add_spec(nm, nr, nc, value_type = "continuous", save_mode = save_mode, line = line)
      next
    }

    if (grepl("^[[:space:]]*(?:param|dparam)\\b", line, perl = TRUE)) {
      stop(
        "Invalid parameter declaration: ", trimws(line),
        ". Supported output syntax is an optional `save=mean` suffix, e.g. ",
        "`param u(m, 2) save=mean;`.",
        call. = FALSE
      )
    }
  }

  if (length(specs)) {
    raw_names <- vapply(specs, `[[`, character(1), "name")
    if (anyDuplicated(raw_names)) stop("Parameter block names in `param ...` / `dparam ...` declarations must be unique.", call. = FALSE)
  }
  is_mean <- vapply(specs, function(item) identical(item$save, "mean"), logical(1))
  list(
    spec = specs,
    dim = offset,
    names = names_out,
    chain_names = unlist(lapply(specs[!is_mean], `[[`, "names"), use.names = FALSE),
    mean_names = unlist(lapply(specs[is_mean], `[[`, "names"), use.names = FALSE),
    mean_ranges = lapply(specs[is_mean], function(item) {
      list(offset = as.integer(item$offset), len = as.integer(item$len), name = item$name)
    })
  )
}

resolve_integer_extent <- function(x, data = NULL, context = "dparam declaration") {
  x <- trimws(x)
  if (grepl("^-?[0-9]+$", x, perl = TRUE)) return(as.integer(x))
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", x, perl = TRUE)) {
    stop("Invalid integer extent `", x, "` in ", context,
         ". Use an integer or the name of a scalar in `data`.", call. = FALSE)
  }
  if (!is.list(data) || is.null(names(data)) || !(x %in% names(data))) {
    stop("Integer extent `", x, "` in ", context,
         " must be supplied as a scalar in `data = list(...)`.", call. = FALSE)
  }
  val <- data[[x]]
  if (!(is.numeric(val) || is.integer(val)) || length(val) != 1L || is.na(val) || !is.finite(val)) {
    stop("Integer extent `", x, "` must be a single finite numeric/integer scalar.", call. = FALSE)
  }
  if (abs(val - round(val)) > .Machine$double.eps^0.5) {
    stop("Integer extent `", x, "` must be integer-like.", call. = FALSE)
  }
  as.integer(round(val))
}


resolve_param_extent <- function(x, data = NULL, context = "param declaration") {
  x <- trimws(x)
  if (grepl("^[0-9]+$", x, perl = TRUE)) return(as.integer(x))
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", x, perl = TRUE)) {
    stop("Invalid parameter extent `", x, "` in ", context,
         ". Use a positive integer or the name of a scalar in `data`.", call. = FALSE)
  }
  if (!is.list(data) || is.null(names(data)) || !(x %in% names(data))) {
    stop("Parameter extent `", x, "` in ", context,
         " must be supplied as a scalar in `data = list(...)`.", call. = FALSE)
  }
  val <- data[[x]]
  if (!(is.numeric(val) || is.integer(val)) || length(val) != 1L || is.na(val) || !is.finite(val)) {
    stop("Parameter extent `", x, "` must be a single finite numeric/integer scalar.", call. = FALSE)
  }
  if (val <= 0 || abs(val - round(val)) > .Machine$double.eps^0.5) {
    stop("Parameter extent `", x, "` must be a positive integer-like scalar.", call. = FALSE)
  }
  as.integer(round(val))
}

strip_param_declarations <- function(src) {
  pat <- paste0(
    "^[[:space:]]*(?:param|dparam)[[:space:]]+[A-Za-z_][A-Za-z0-9_]*",
    "[[:space:]]*\\([^)]+\\)",
    "(?:[[:space:]]+save[[:space:]]*=[[:space:]]*mean)?",
    "[[:space:]]*;?[[:space:]]*(?://.*)?$"
  )
  src[!grepl(pat, src, perl = TRUE)]
}


find_log_posterior_param_name <- function(src) {
  txt <- paste(src, collapse = "\n")
  pats <- c(
    "double[[:space:]]+log_posterior[[:space:]]*\\([[:space:]]*const[[:space:]]+double[[:space:]]*\\*[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*,[[:space:]]*int[[:space:]]+[A-Za-z_][A-Za-z0-9_]*[[:space:]]*\\)",
    "double[[:space:]]+log_posterior[[:space:]]*\\([[:space:]]*double[[:space:]]+const[[:space:]]*\\*[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*,[[:space:]]*int[[:space:]]+[A-Za-z_][A-Za-z0-9_]*[[:space:]]*\\)",
    "double[[:space:]]+posterior_logp[[:space:]]*\\([[:space:]]*const[[:space:]]+double[[:space:]]*\\*[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*,[[:space:]]*int[[:space:]]+[A-Za-z_][A-Za-z0-9_]*[[:space:]]*\\)"
  )
  for (pat in pats) {
    m <- regexec(pat, txt, perl = TRUE)
    hit <- regmatches(txt, m)[[1L]]
    if (length(hit) == 2L) return(hit[[2L]])
  }
  NULL
}

generate_param_macros <- function(param_info, theta_name) {
  if (is.null(param_info) || !length(param_info$spec)) return(character())
  if (is.null(theta_name) || !nzchar(theta_name)) theta_name <- "theta"
  out <- c("", "/* generated by hobbs from param declarations */")
  for (item in param_info$spec) {
    nm <- item$name
    off <- as.integer(item$offset)
    nr <- as.integer(item$nrow)
    nc <- as.integer(item$ncol)
    is_discrete <- identical(item$value_type %||% "continuous", "discrete")
    if (nc == 1L) {
      if (is_discrete) {
        # One-based integer vector access for dparam: z(i) == (int)theta[offset + i - 1]
        out <- c(out, sprintf("#define %s(i) ((int)%s[%d + ((int)(i) - 1)])", nm, theta_name, off))
      } else {
        # One-based vector access: beta(i) == theta[offset + i - 1]
        out <- c(out, sprintf("#define %s(i) %s[%d + ((int)(i) - 1)]", nm, theta_name, off))
      }
    } else {
      if (is_discrete) {
        out <- c(out, sprintf("#define %s(i, j) ((int)%s[%d + ((int)(i) - 1) + ((int)(j) - 1) * %d])", nm, theta_name, off, nr))
      } else {
        # One-based, R-style column-major matrix access.
        out <- c(out, sprintf("#define %s(i, j) %s[%d + ((int)(i) - 1) + ((int)(j) - 1) * %d]", nm, theta_name, off, nr))
      }
    }
  }
  c(out, "")
}

translate_user_model_c <- function(model_c, workdir, param_info = NULL, block_info = NULL) {
  src <- readLines(model_c, warn = FALSE)
  src <- normalize_attached_cache_syntax(src)
  src <- expand_multi_block_declarations(src)
  extracted_cache <- extract_attached_cache_declarations(src)
  src <- extracted_cache$src
  src <- add_implicit_block_target(src)
  src <- strip_param_declarations(src)
  translated <- translate_simple_model_signature(src)
  theta_name <- attr(translated, "theta_name", exact = TRUE) %||% find_log_posterior_param_name(translated)
  if (is.null(theta_name) || !nzchar(theta_name)) theta_name <- "theta"
  macros <- generate_param_macros(param_info, theta_name)
  cache_c <- generate_cache_c(extracted_cache$caches, param_info, theta_name)
  translated <- c(macros, cache_c, translated)
  translated <- translate_block_signatures(translated, theta_name, param_info = param_info)
  translated <- translate_distribution_statements(translated)
  translated <- translate_vec_declarations(translated)
  translated <- translate_mat_declarations(translated)
  translated <- translate_r_like_for_loops(translated)
  translated <- vectorize_simple_target_reductions(translated)

  # These plain-C wrappers are intentionally emitted last. The compiler can
  # inline each model block and its deterministic cache update into one native
  # scalar sweep, leaving only one Rust/C boundary crossing per declared
  # parameter vector per MCMC sweep.
  runtime_c <- generate_scalar_kernels_c(param_info, block_info, extracted_cache$caches)
  translated <- c(translated, runtime_c)
  out <- file.path(workdir, "posterior_model_user_translated.c")
  writeLines(translated, out)
  normalizePath(out, mustWork = TRUE)
}


translate_block_signatures <- function(src, theta_name = "par", param_info = NULL) {
  pmap <- list()
  if (!is.null(param_info) && length(param_info$spec)) {
    pmap <- setNames(param_info$spec, vapply(param_info$spec, `[[`, character(1), "name"))
  }
  vapply(src, function(line) {
    pat_multi <- "^([[:space:]]*)block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([^)]*,[^)]*)[[:space:]]*\\)[[:space:]]*\\{(.*)$"
    m <- regexec(pat_multi, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 5L) {
      indent <- hit[[2L]]
      block_name <- hit[[3L]]
      idx_names <- split_top_level_commas(hit[[4L]])
      if (length(idx_names) != 2L || any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", idx_names, perl = TRUE))) {
        stop("Multi-index block syntax currently supports exactly two index variables, e.g. `block u(j, l) { ... }`.", call. = FALSE)
      }
      info <- pmap[[block_name]]
      if (is.null(info) || as.integer(info$ncol) <= 1L) {
        stop("`block ", block_name, "(", paste(idx_names, collapse = ", "), ")` requires a two-dimensional `param ", block_name, "(nrow, ncol)` declaration.", call. = FALSE)
      }
      nr <- as.integer(info$nrow)
      rest <- hit[[5L]]
      paste0(indent, "double hobbs_block_", block_name, "(const double *", theta_name, ", int hobbs_block_index) {",
             " (void)", theta_name, "; (void)hobbs_block_index;",
             " int ", idx_names[[1L]], " = ((hobbs_block_index - 1) % ", nr, ") + 1;",
             " int ", idx_names[[2L]], " = ((hobbs_block_index - 1) / ", nr, ") + 1;",
             " (void)", idx_names[[1L]], "; (void)", idx_names[[2L]], ";", rest)
    } else {
      pat_index <- "^([[:space:]]*)block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\)[[:space:]]*\\{(.*)$"
      m <- regexec(pat_index, line, perl = TRUE)
      hit <- regmatches(line, m)[[1L]]
      if (length(hit) == 5L) {
        return(paste0(hit[[2L]], "double hobbs_block_", hit[[3L]], "(const double *", theta_name, ", int ", hit[[4L]], ") { (void)", theta_name, "; (void)", hit[[4L]], ";", hit[[5L]]))
      }

      pat_one <- "^([[:space:]]*)block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*1[[:space:]]*\\)[[:space:]]*\\{(.*)$"
      m <- regexec(pat_one, line, perl = TRUE)
      hit <- regmatches(line, m)[[1L]]
      if (length(hit) == 4L) {
        return(paste0(hit[[2L]], "double hobbs_block_", hit[[3L]], "(const double *", theta_name, ", int __hobbs_block_index_unused) { (void)", theta_name, "; (void)__hobbs_block_index_unused;", hit[[4L]]))
      }

      pat_group <- "^([[:space:]]*)block[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\{(.*)$"
      m <- regexec(pat_group, line, perl = TRUE)
      hit <- regmatches(line, m)[[1L]]
      if (length(hit) == 4L) {
        return(paste0(hit[[2L]], "double hobbs_block_", hit[[3L]], "(const double *", theta_name, ", int __hobbs_block_index_unused) { (void)", theta_name, "; (void)__hobbs_block_index_unused;", hit[[4L]]))
      }
      line
    }
  }, character(1), USE.NAMES = FALSE)
}

translate_vec_declarations <- function(src) {
  # Local convenience vectors in user model/block code:
  #   vec x(2);
  # becomes a stack array initialized to zero, and x(i) on ordinary lines is
  # rewritten to one-based array indexing.  Distribution range LHS rewriting is
  # run before this function, so x(1:2) ~ dmvn(...) is already pointer-based.
  decl_pat <- "^([[:space:]]*)vec[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([^)]+?)[[:space:]]*\\)[[:space:]]*;[[:space:]]*(?://.*)?$"
  vec_names <- character()
  for (line in src) {
    m <- regexec(decl_pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 4L) vec_names <- c(vec_names, hit[[3L]])
  }
  out <- character()
  for (line in src) {
    m <- regexec(decl_pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 4L) {
      indent <- hit[[2L]]
      nm <- hit[[3L]]
      n <- trimws(hit[[4L]])
      init_i <- paste0("__hobbs_vec_i_", nm)
      out <- c(out,
        paste0(indent, "double ", nm, "[(int)(", n, ")];"),
        paste0(indent, "for (int ", init_i, " = 0; ", init_i, " < (int)(", n, "); ++", init_i, ") ", nm, "[", init_i, "] = 0.0;")
      )
    } else {
      if (length(vec_names)) {
        for (nm in vec_names) {
          pat <- paste0("\\b", nm, "[[:space:]]*\\([[:space:]]*([^():]+?)[[:space:]]*\\)")
          line <- gsub(pat, paste0(nm, "[((int)(\\1) - 1)]"), line, perl = TRUE)
        }
      }
      out <- c(out, line)
    }
  }
  out
}


translate_mat_declarations <- function(src) {
  # Local convenience matrices in user model/block code:
  #   mat A(2, 2);
  # becomes a stack array initialized to zero, and A(i, j) on ordinary lines is
  # rewritten to one-based, R-style column-major array indexing.  Distribution
  # range LHS rewriting is run before this function, so A(1:2, 1:2) patterns
  # are not handled here.
  decl_pat <- "^([[:space:]]*)mat[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([^,]+?)[[:space:]]*,[[:space:]]*([^)]+?)[[:space:]]*\\)[[:space:]]*;[[:space:]]*(?://.*)?$"
  mats <- list()
  for (line in src) {
    m <- regexec(decl_pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 5L) {
      mats[[hit[[3L]]]] <- list(nrow = trimws(hit[[4L]]), ncol = trimws(hit[[5L]]))
    }
  }
  out <- character()
  for (line in src) {
    m <- regexec(decl_pat, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 5L) {
      indent <- hit[[2L]]
      nm <- hit[[3L]]
      nr <- trimws(hit[[4L]])
      nc <- trimws(hit[[5L]])
      init_i <- paste0("__hobbs_mat_i_", nm)
      out <- c(out,
        paste0(indent, "double ", nm, "[(int)(", nr, ") * (int)(", nc, ")];"),
        paste0(indent, "for (int ", init_i, " = 0; ", init_i, " < (int)(", nr, ") * (int)(", nc, "); ++", init_i, ") ", nm, "[", init_i, "] = 0.0;")
      )
    } else {
      if (length(mats)) {
        for (nm in names(mats)) {
          nr <- mats[[nm]]$nrow
          pat <- paste0("\\b", nm, "[[:space:]]*\\([[:space:]]*([^(),:]+?)[[:space:]]*,[[:space:]]*([^(),:]+?)[[:space:]]*\\)")
          repl <- paste0(nm, "[((int)(\\1) - 1) + ((int)(\\2) - 1) * (int)(", nr, ")]")
          line <- gsub(pat, repl, line, perl = TRUE)
        }
      }
      out <- c(out, line)
    }
  }
  out
}


translate_simple_model_signature <- function(src) {
  # Translate the preferred hobbs model syntax:
  #   my_model {
  #       ...
  #   }
  # to:
  #   double log_posterior(const double *theta, int dim) {
  #       ...
  #   }
  # Also keeps supporting:
  #   my_model(theta) { ... }
  # where the user chooses the parameter-vector name.  Helper functions should
  # use ordinary C signatures.  If no model function is present, block-only
  # models are allowed later when update = "block".
  control_words <- c("if", "for", "while", "switch", "return", "sizeof", "block", "param")
  pat_arg <- "^([[:space:]]*)([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\)[[:space:]]*\\{(.*)$"
  pat_noarg <- "^([[:space:]]*)([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\{(.*)$"
  done <- FALSE
  out <- src
  for (k in seq_along(out)) {
    if (done) break
    line <- out[[k]]

    m <- regexec(pat_arg, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 5L) {
      indent <- hit[[2L]]
      model_name <- hit[[3L]]
      param_name <- hit[[4L]]
      rest <- hit[[5L]]
      if (!(model_name %in% control_words)) {
        if (identical(param_name, "dim")) {
          stop("The parameter-vector name in `model_name(param_name) { ... }` cannot be `dim`, because hobbs supplies `dim` separately.", call. = FALSE)
        }
        out[[k]] <- paste0(indent, "double log_posterior(const double *", param_name, ", int dim) { (void)dim;", rest)
        attr(out, "theta_name") <- param_name
        done <- TRUE
      }
      next
    }

    m <- regexec(pat_noarg, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit) == 4L) {
      indent <- hit[[2L]]
      model_name <- hit[[3L]]
      rest <- hit[[4L]]
      if (!(model_name %in% control_words)) {
        out[[k]] <- paste0(indent, "double log_posterior(const double *theta, int dim) { (void)dim;", rest)
        attr(out, "theta_name") <- "theta"
        done <- TRUE
      }
    }
  }
  out
}


translate_distribution_statements <- function(src) {
  vapply(src, translate_distribution_statement_line, character(1), USE.NAMES = FALSE)
}

translate_distribution_statement_line <- function(line) {
  # Convenience BUGS/JAGS-like density statements.  These are pure text
  # rewrites to the same cached helpers users can call manually, so there is no
  # runtime penalty versus `target += ..._lpdf(...)`.
  if (!grepl("~", line, fixed = TRUE)) return(line)

  # Preserve a compact one-line loop such as
  #   for (j = 1:p) beta(j) ~ dnorm(0, 1);
  # by translating only its density statement. The loop itself is converted by
  # translate_r_like_for_loops() later in the pipeline.
  control_pat <- "^([[:space:]]*for[[:space:]]*\\([^)]*\\)[[:space:]]*)(.+~.+;[[:space:]]*(?://.*)?)$"
  control_match <- regexec(control_pat, line, perl = TRUE)
  control_hit <- regmatches(line, control_match)[[1L]]
  if (length(control_hit) == 3L) {
    translated_body <- translate_distribution_statement_line(control_hit[[3L]])
    return(paste0(control_hit[[2L]], trimws(translated_body)))
  }

  pat <- "^([[:space:]]*)([^~;]+?)[[:space:]]*~[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\((.*)\\)[[:space:]]*;[[:space:]]*(?://.*)?$"
  m <- regexec(pat, line, perl = TRUE)
  hit <- regmatches(line, m)[[1L]]
  if (length(hit) != 5L) return(line)
  indent <- hit[[2L]]
  lhs <- trimws(hit[[3L]])
  dist <- hit[[4L]]
  args <- trimws(hit[[5L]])

  lhs_range <- parse_lhs_range(lhs)
  if (dist %in% c("dmvn", "mvn", "multi_normal", "multivariate_normal")) {
    if (is.null(lhs_range)) return(line)
    return(paste0(indent, "target += dmvn_lpdf(", lhs_range$ptr, ", ", args, ", ", lhs_range$dim, ");"))
  }
  if (dist %in% c("dbvn", "bvn", "bivariate_normal")) {
    if (!is.null(lhs_range)) {
      return(paste0(indent, "target += dbvn_cov_lpdf(", lhs_range$ptr, ", ", args, ");"))
    }
    return(paste0(indent, "target += dbvn_lpdf(", lhs, if (nzchar(args)) ", " else "", args, ");"))
  }
  if (dist %in% c("dwish", "wishart", "dinvwish", "invwishart", "inv_wishart", "dlkjcorr2", "lkjcorr2")) {
    if (is.null(lhs_range)) return(line)
    fn_mat <- switch(dist,
      dwish = "dwish_lpdf",
      wishart = "dwish_lpdf",
      dinvwish = "dinvwish_lpdf",
      invwishart = "dinvwish_lpdf",
      inv_wishart = "dinvwish_lpdf",
      dlkjcorr2 = "dlkjcorr2_lpdf",
      lkjcorr2 = "dlkjcorr2_lpdf",
      NA_character_
    )
    return(paste0(indent, "target += ", fn_mat, "(", lhs_range$ptr, if (nzchar(args)) ", " else "", args, ");"))
  }

  fn <- switch(dist,
    dbern = "bernoulli_lpdf",
    bernoulli = "bernoulli_lpdf",
    bernoulli_logit = "bernoulli_logit_lpdf",
    bernoulli_probit = "bernoulli_probit_lpdf",
    bernoulli_cloglog = "bernoulli_cloglog_lpdf",
    dpois = "poisson_lpdf",
    poisson = "poisson_lpdf",
    poisson_log = "poisson_log_lpdf",
    dnorm = "normal_lpdf",
    normal = "normal_lpdf",
    normal01 = "normal01_lpdf",
    normal_sd1 = "normal_sd1_lpdf",
    dbinom = "binomial_lpdf",
    binomial = "binomial_lpdf",
    binomial_logit = "binomial_logit_lpdf",
    dnbinom = "negbinomial_lpdf",
    negbinomial = "negbinomial_lpdf",
    negbinomial_log = "negbinomial_log_lpdf",
    dnbinom_log = "negbinomial_log_lpdf",
    dunif = "uniform_lpdf",
    uniform = "uniform_lpdf",
    dexp = "exponential_lpdf",
    exponential = "exponential_lpdf",
    dgamma = "gamma_lpdf",
    gamma = "gamma_lpdf",
    dinvgamma = "invgamma_lpdf",
    invgamma = "invgamma_lpdf",
    dbeta = "beta_lpdf",
    beta = "beta_lpdf",
    dcauchy = "cauchy_lpdf",
    cauchy = "cauchy_lpdf",
    dt = "student_t_lpdf",
    student_t = "student_t_lpdf",
    dchisq = "chisq_lpdf",
    chisq = "chisq_lpdf",
    dlnorm = "lognormal_lpdf",
    lognormal = "lognormal_lpdf",
    dlogis = "logistic_lpdf",
    logistic = "logistic_lpdf",
    dlaplace = "laplace_lpdf",
    laplace = "laplace_lpdf",
    dweibull = "weibull_lpdf",
    weibull = "weibull_lpdf",
    dpareto = "pareto_lpdf",
    pareto = "pareto_lpdf",
    dhalfnorm = "halfnorm_lpdf",
    halfnorm = "halfnorm_lpdf",
    dhalfcauchy = "halfcauchy_lpdf",
    halfcauchy = "halfcauchy_lpdf",
    NA_character_
  )
  if (is.na(fn)) return(line)
  paste0(indent, "target += ", fn, "(", lhs, if (nzchar(args)) ", " else "", args, ");")
}

parse_lhs_range <- function(lhs) {
  # Range sampling syntax for contiguous local vectors/arrays, e.g.
  #   utem(1:2) ~ dmvn(mu, Sigma);
  pat <- "^([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([A-Za-z_][A-Za-z0-9_]*|[0-9]+)[[:space:]]*:[[:space:]]*([A-Za-z_][A-Za-z0-9_]*|[0-9]+)[[:space:]]*\\)$"
  m <- regexec(pat, lhs, perl = TRUE)
  hit <- regmatches(lhs, m)[[1L]]
  if (length(hit) == 4L) {
    nm <- hit[[2L]]
    lo <- hit[[3L]]
    hi <- hit[[4L]]
    dim <- if (grepl("^[0-9]+$", lo) && grepl("^[0-9]+$", hi)) {
      as.character(as.integer(hi) - as.integer(lo) + 1L)
    } else {
      paste0("((int)(", hi, ") - (int)(", lo, ") + 1)")
    }
    return(list(ptr = paste0(nm, " + ((int)(", lo, ") - 1)"), dim = dim))
  }

  # Matrix row/column slices with fixed integer ranges, e.g.
  #   u(j, 1:2) ~ dmvn(mu, Sigma);
  # dmvn_lpdf expects contiguous input, while a matrix row is strided in the
  # generated column-major layout.  Use a C99 compound literal so users do not
  # have to create an explicit temporary vector in the model code.
  pat2 <- "^([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\([[:space:]]*([^,]+?)[[:space:]]*,[[:space:]]*([^,]+?)[[:space:]]*\\)$"
  m <- regexec(pat2, lhs, perl = TRUE)
  hit <- regmatches(lhs, m)[[1L]]
  if (length(hit) != 4L) return(NULL)

  nm <- hit[[2L]]
  a <- trimws(hit[[3L]])
  b <- trimws(hit[[4L]])
  a_colon <- find_top_level_colon(a)
  b_colon <- find_top_level_colon(b)

  if (!is.na(a_colon) && is.na(b_colon)) {
    lo <- trimws(substring(a, 1L, a_colon - 1L))
    hi <- trimws(substring(a, a_colon + 1L))
    if (!grepl("^[0-9]+$", lo) || !grepl("^[0-9]+$", hi)) return(NULL)
    idx <- seq.int(as.integer(lo), as.integer(hi))
    vals <- paste0(nm, "(", idx, ",", b, ")")
    return(list(ptr = paste0("((double[]){", paste(vals, collapse = ", "), "})"), dim = as.character(length(idx))))
  }

  if (is.na(a_colon) && !is.na(b_colon)) {
    lo <- trimws(substring(b, 1L, b_colon - 1L))
    hi <- trimws(substring(b, b_colon + 1L))
    if (!grepl("^[0-9]+$", lo) || !grepl("^[0-9]+$", hi)) return(NULL)
    idx <- seq.int(as.integer(lo), as.integer(hi))
    vals <- paste0(nm, "(", a, ",", idx, ")")
    return(list(ptr = paste0("((double[]){", paste(vals, collapse = ", "), "})"), dim = as.character(length(idx))))
  }

  NULL
}

translate_r_like_for_loops <- function(src) {
  vapply(src, translate_r_like_for_loop_line, character(1), USE.NAMES = FALSE)
}


vectorize_simple_target_reductions <- function(src) {
  # Break the long floating-point dependency chain in the common generated
  # pattern
  #   for (int i = 1; i <= n; ++i) {
  #     target += expression(i);
  #   }
  # using eight independent accumulators. This is deliberately conservative:
  # only an otherwise empty three-line loop is transformed.
  out <- character()
  i <- 1L
  serial <- 0L
  loop_pat <- "^([[:space:]]*)for[[:space:]]*\\([[:space:]]*int[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*=[[:space:]]*1[[:space:]]*;[[:space:]]*\\2[[:space:]]*<=[[:space:]]*(.+?)[[:space:]]*;[[:space:]]*\\+\\+\\2[[:space:]]*\\)[[:space:]]*\\{[[:space:]]*$"
  add_pat <- "^[[:space:]]*target[[:space:]]*\\+=[[:space:]]*(.+);[[:space:]]*$"
  close_pat <- "^[[:space:]]*}[[:space:]]*$"
  while (i <= length(src)) {
    if (i + 2L <= length(src)) {
      m <- regexec(loop_pat, src[[i]], perl = TRUE)
      loop_hit <- regmatches(src[[i]], m)[[1L]]
      a <- regexec(add_pat, src[[i + 1L]], perl = TRUE)
      add_hit <- regmatches(src[[i + 1L]], a)[[1L]]
      if (length(loop_hit) == 4L && length(add_hit) == 2L && grepl(close_pat, src[[i + 2L]], perl = TRUE)) {
        indent <- loop_hit[[2L]]
        var <- loop_hit[[3L]]
        upper <- trimws(loop_hit[[4L]])
        expr <- trimws(add_hit[[2L]])
        # Avoid transforming expressions with obvious side effects.
        if (!grepl("(\\+\\+|--|[^=!<>]=[^=])", expr, perl = TRUE)) {
          serial <- serial + 1L
          tag <- paste0("hobbs_red_", serial)
          replace_var <- function(offset) {
            repl <- if (offset == 0L) var else sprintf("(%s + %d)", var, offset)
            gsub(paste0("\\b", var, "\\b"), repl, expr, perl = TRUE)
          }
          lines <- c(
            paste0(indent, "{"),
            sprintf("%s  const int64_t %s_end = (int64_t)(%s);", indent, tag, upper),
            sprintf("%s  int64_t %s = 1;", indent, var),
            sprintf("%s  double %s_0 = 0.0, %s_1 = 0.0, %s_2 = 0.0, %s_3 = 0.0;", indent, tag, tag, tag, tag),
            sprintf("%s  double %s_4 = 0.0, %s_5 = 0.0, %s_6 = 0.0, %s_7 = 0.0;", indent, tag, tag, tag, tag),
            sprintf("%s  const int64_t %s_vec_end = %s_end >= 8 ? %s_end - 7 : 0;", indent, tag, tag, tag),
            sprintf("%s  for (; %s <= %s_vec_end; %s += 8) {", indent, var, tag, var)
          )
          for (k in 0:7) {
            lines <- c(lines, sprintf("%s    %s_%d += %s;", indent, tag, k, replace_var(k)))
          }
          lines <- c(
            lines,
            paste0(indent, "  }"),
            sprintf("%s  for (; %s < %s_end; ++%s) {", indent, var, tag, var),
            sprintf("%s    %s_0 += %s;", indent, tag, replace_var(0L)),
            paste0(indent, "  }"),
            sprintf("%s  if (%s == %s_end) %s_0 += %s;", indent, var, tag, tag, replace_var(0L)),
            sprintf("%s  target += ((%s_0 + %s_1) + (%s_2 + %s_3)) + ((%s_4 + %s_5) + (%s_6 + %s_7));", indent, tag, tag, tag, tag, tag, tag, tag, tag),
            paste0(indent, "}")
          )
          out <- c(out, lines)
          i <- i + 3L
          next
        }
      }
    }
    out <- c(out, src[[i]])
    i <- i + 1L
  }
  out
}

translate_r_like_for_loop_line <- function(line) {
  # Translate the hobbs model convenience syntax:
  #   for (i in 1:N) { ... }
  #   for (i = 1:N)  { ... }
  # into ordinary C:
  #   for (int i = 1; i <= N; ++i) { ... }
  # This is intentionally conservative.  Normal C for-loops contain semicolons
  # and are left untouched.
  loc <- regexpr("for\\s*\\(", line, perl = TRUE)
  if (loc[[1L]] < 0L) return(line)

  open <- regexpr("\\(", substring(line, loc[[1L]]), perl = TRUE)[[1L]] + loc[[1L]] - 1L
  chars <- strsplit(line, "", fixed = TRUE)[[1L]]
  depth <- 0L
  close <- NA_integer_
  for (k in seq.int(open, length(chars))) {
    if (identical(chars[[k]], "(")) depth <- depth + 1L
    if (identical(chars[[k]], ")")) {
      depth <- depth - 1L
      if (depth == 0L) {
        close <- k
        break
      }
    }
  }
  if (is.na(close)) return(line)

  inner <- substring(line, open + 1L, close - 1L)
  if (grepl(";", inner, fixed = TRUE)) return(line)

  m <- regexec("^\\s*(?:int\\s+)?([A-Za-z_][A-Za-z0-9_]*)\\s*(?:in|=)\\s*(.+)\\s*$", inner, perl = TRUE)
  hit <- regmatches(inner, m)[[1L]]
  if (length(hit) != 3L) return(line)

  var <- trimws(hit[[2L]])
  range <- trimws(hit[[3L]])
  colon <- find_top_level_colon(range)
  if (is.na(colon)) return(line)

  lower <- trimws(substring(range, 1L, colon - 1L))
  upper <- trimws(substring(range, colon + 1L))
  if (!nzchar(lower) || !nzchar(upper)) return(line)

  repl <- sprintf("for (int %s = %s; %s <= %s; ++%s)", var, lower, var, upper, var)
  paste0(substring(line, 1L, loc[[1L]] - 1L), repl, substring(line, close + 1L))
}

find_top_level_colon <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  depth <- 0L
  for (k in seq_along(chars)) {
    ch <- chars[[k]]
    if (identical(ch, "(")) depth <- depth + 1L
    else if (identical(ch, ")")) depth <- depth - 1L
    else if (identical(ch, ":") && depth == 0L) return(k)
  }
  NA_integer_
}

prepare_model_translation_unit <- function(model_c, workdir, log_cache = FALSE, data_spec = NULL, param_info = NULL, block_info = NULL, allow_block_only = FALSE) {
  model_c <- translate_user_model_c(model_c, workdir, param_info = param_info, block_info = block_info)
  src <- readLines(model_c, warn = FALSE)
  has_posterior_logp <- any(grepl("\\bposterior_logp\\s*\\(", src))
  has_log_posterior <- any(grepl("\\blog_posterior\\s*\\(", src))
  has_posterior_init <- any(grepl("\\bposterior_init\\s*\\(", src))
  has_posterior_free <- any(grepl("\\bposterior_free\\s*\\(", src))
  if (!is.null(data_spec) && (has_posterior_init || has_posterior_free)) {
    stop("When `data` is an R object, the C model should not define `posterior_init` or `posterior_free`; hobbs generates those automatically.", call. = FALSE)
  }

  include_dir <- system.file("include", package = "hobbs", mustWork = FALSE)
  if (!nzchar(include_dir) || !dir.exists(include_dir)) {
    include_dir <- normalizePath(file.path(dirname(dirname(model_c)), "inst", "include"), mustWork = FALSE)
  }
  header <- file.path(include_dir, "hobbs_model.h")

  wrapper <- file.path(workdir, "posterior_model_wrapped.c")
  data_bridge <- if (!is.null(data_spec)) c("", generate_data_bridge_c(data_spec, row_map_spec = attr(data_spec, "hobbs_row_map_spec", exact = TRUE)), "") else character()
  lines <- c(
    "/* generated by hobbs: helper API + optional ABI wrapper */",
    sprintf("#include %s", encode_c_include(header)),
    data_bridge,
    sprintf("#include %s", encode_c_include(model_c))
  )

  if (!has_posterior_logp) {
    if (!has_log_posterior) {
      if (!isTRUE(allow_block_only)) {
        stop("C model must define preferred syntax `model_name { ... }` or `model_name(theta) { ... }`, ",
             "legacy `double log_posterior(const double *theta, int dim)`, ",
             "or advanced ABI function `double posterior_logp(const double *theta, int dim)`.",
             call. = FALSE)
      }
    } else {
      lines <- c(lines,
        "",
        "double posterior_logp(const double *theta, int dim) {",
        "    return log_posterior(theta, dim);",
        "}")
    }
  }

  writeLines(lines, wrapper)
  normalizePath(wrapper, mustWork = TRUE)
}

encode_c_include <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  paste0('"', gsub("\\\\", "/", path), '"')
}

compile_c_model <- function(model_c, workdir, compiler = NULL, cflags = NULL, quiet = FALSE, log_cache = list(enabled = FALSE)) {
  sys <- Sys.info()[["sysname"]]
  if (is.null(compiler)) {
    compiler <- if (identical(sys, "Darwin")) {
      c("clang", "cc", "gcc")
    } else if (.Platform$OS.type == "windows") {
      c("gcc", "clang", "cc")
    } else {
      c("cc", "gcc", "clang")
    }
  }
  compiler <- unname(Sys.which(compiler))
  compiler <- compiler[nzchar(compiler)]
  if (!length(compiler)) stop("Could not find requested C compiler", call. = FALSE)
  compiler <- compiler[[1L]]

  ext <- if (identical(sys, "Darwin")) ".dylib" else if (.Platform$OS.type == "windows") ".dll" else ".so"
  lib <- file.path(workdir, paste0("posterior", ext))

  if (is.null(cflags)) {
    opt_flags <- c("-std=gnu11", "-O3", "-DNDEBUG", "-march=native", "-flto", "-fno-math-errno", "-ffp-contract=fast", "-funroll-loops")
    cflags <- if (identical(sys, "Darwin")) {
      c(opt_flags, "-dynamiclib")
    } else if (.Platform$OS.type == "windows") {
      c(opt_flags, "-shared", "-static-libgcc", "-Wl,--export-all-symbols")
    } else {
      c(opt_flags, "-fno-semantic-interposition", "-fPIC", "-shared", "-Wl,-O2")
    }
  }

  include_dir <- system.file("include", package = "hobbs", mustWork = FALSE)
  if (!nzchar(include_dir) || !dir.exists(include_dir)) {
    include_dir <- normalizePath(file.path(dirname(dirname(model_c)), "inst", "include"), mustWork = FALSE)
  }
  cache_flags <- character()
  if (isTRUE(log_cache$enabled)) {
    cache_flags <- c(
      "-Dhobbs_ENABLE_LOG_CACHE",
      paste0("-Dhobbs_LOG_CACHE_BITS=", as.integer(log_cache$bits)),
      paste0("-Dhobbs_LOGIT_CACHE_BITS=", as.integer(log_cache$bits))
    )
  }
  ldflags <- if (!identical(sys, "Darwin") && !identical(.Platform$OS.type, "windows")) "-lm" else character()
  args <- c(cflags, cache_flags, paste0("-I", shQuote(include_dir)), shQuote(model_c), "-o", shQuote(lib), ldflags)
  status <- system2(compiler, args, stdout = if (quiet) FALSE else "", stderr = if (quiet) FALSE else "")
  if (!identical(status, 0L)) stop("C model compilation failed", call. = FALSE)
  normalizePath(lib, mustWork = TRUE)
}


validate_log_cache <- function(log_cache, log_cache_bits) {
  enabled <- isTRUE(log_cache)
  if (is.list(log_cache)) {
    enabled <- isTRUE(log_cache$enabled %||% TRUE)
    log_cache_bits <- log_cache$bits %||% log_cache_bits
  }

  bits <- as.integer(log_cache_bits)
  if (length(bits) != 1L || is.na(bits) || bits < 8L || bits > 28L) {
    stop("`log_cache_bits` must be a single integer from 8 to 28.", call. = FALSE)
  }
  list(
    enabled = enabled,
    bits = bits,
    entries = 2^bits,
    approximate = enabled
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

format_num <- function(x) format(x, scientific = FALSE, trim = TRUE)

example_scalar_code <- function() {
'double log_posterior(const double* theta, int dim) {
    if (dim < 2) return -INFINITY;
    const double x = theta[0];
    const double y = theta[1];
    const double lp_x = -0.5 * (x * x) / 100.0;
    const double mean_y = 0.03 * (x * x - 100.0);
    const double r = y - mean_y;
    double target = lp_x -0.5 * (r * r) / 4.0;
    for (int j = 2; j < dim; ++j) target += -0.5 * theta[j] * theta[j];
    return target;
}
'
}

example_batch_code <- function() {
'static inline double one_logp(const double* theta, int dim) {
    if (dim < 2) return -INFINITY;
    const double x = theta[0];
    const double y = theta[1];
    const double lp_x = -0.5 * (x * x) / 100.0;
    const double mean_y = 0.03 * (x * x - 100.0);
    const double r = y - mean_y;
    double target = lp_x -0.5 * (r * r) / 4.0;
    for (int j = 2; j < dim; ++j) target += -0.5 * theta[j] * theta[j];
    return target;
}

double log_posterior(const double* theta, int dim) {
    return one_logp(theta, dim);
}

void posterior_logp_batch(const double* theta, int dim, int n_batch, double* out) {
    for (int i = 0; i < n_batch; ++i) out[i] = one_logp(theta + ((long long)i * dim), dim);
}
'
}

print.hobbs_run <- function(x, ...) {
  cat("hobbs run\n")
  has_chain_output <- !is.null(x$chain_output) && length(x$chain_output) == 1L &&
    !is.na(x$chain_output) && nzchar(x$chain_output)
  has_mean_output <- !is.null(x$mean_output) && length(x$mean_output) == 1L &&
    !is.na(x$mean_output) && nzchar(x$mean_output)
  if (has_chain_output) cat("  chain:       ", x$chain_output, "\n", sep = "")
  if (has_mean_output) cat("  mean:        ", x$mean_output, "\n", sep = "")
  if (!has_chain_output && !has_mean_output) cat("  output:      ", x$output, "\n", sep = "")
  if (!is.null(x$data)) cat("  data:        ", x$data, "\n", sep = "")
  cat("  dim:         ", x$dim, "\n", sep = "")
  if (!is.null(x$chain_dim) && !is.null(x$mean_dim) && x$mean_dim > 0L) {
    cat("  retained:    ", x$chain_dim, " full-chain; ", x$mean_dim, " mean-only\n", sep = "")
  }
  cat("  samples:     ", x$samples, "\n", sep = "")
  cat("  warmups:     ", x$warmups %||% x$burnin, "\n", sep = "")
  cat("  adapt_until: ", x$adapt_until, "\n", sep = "")
  cat("  eval:        ", x$eval, "\n", sep = "")
  cat("  save:        ", x$save %||% "chain", "\n", sep = "")
  if (!is.null(x$adaptation) && !is.na(x$adaptation)) {
    cat("  adaptation:  ", x$adaptation, "\n", sep = "")
  }
  invisible(x)
}
