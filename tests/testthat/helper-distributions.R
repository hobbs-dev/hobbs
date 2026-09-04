skip_if_hobbs_toolchain_missing <- function() {
    testthat::skip_if(!nzchar(Sys.which("cargo")), "Cargo is required for hobbs integration tests")
    testthat::skip_if(!nzchar(Sys.which("rustc")), "rustc is required for hobbs integration tests")
    cc <- c(Sys.which("cc"), Sys.which("clang"), Sys.which("gcc"))
    testthat::skip_if(!any(nzchar(cc)), "A C compiler is required for hobbs integration tests")
}

hobbs_test_draws <- function(model, data = list(), seed = 4401L,
                             samples = 5000L, burnin = 1500L) {
    skip_if_hobbs_toolchain_missing()
    if (is.null(data$n) && length(data)) {
        first <- data[[1L]]
        data$n <- if (is.matrix(first) || length(dim(first)) == 2L) nrow(first) else length(first)
    }
    wd <- tempfile("hobbs-dist-test-")
    dir.create(wd)
    on.exit(unlink(wd, recursive = TRUE, force = TRUE), add = TRUE)
    
    fit <- hobbs(
        model = model,
        data = data,
        samples = samples,
        burnin = burnin,
        workdir = wd,
        out = file.path(wd, "chain.bin"),
        seed = seed
    )
    read_hobbs(fit$chain_output)
}

expect_posterior_near <- function(draws, parameter, truth, tolerance) {
    estimate <- mean(draws[, parameter])
    testthat::expect_equal(estimate, truth, tolerance = tolerance)
}

inv_logit_r <- function(x) 1 / (1 + exp(-x))
inv_cloglog_r <- function(x) 1 - exp(-exp(x))

r_laplace <- function(n, location, scale) {
    u <- runif(n, -0.5, 0.5)
    location - scale * sign(u) * log1p(-2 * abs(u))
}

r_pareto <- function(n, xmin, alpha) {
    xmin / runif(n)^(1 / alpha)
}

r_halfnormal <- function(n, sd) abs(rnorm(n, 0, sd))
r_halfcauchy <- function(n, scale) abs(rcauchy(n, 0, scale))

r_invwishart <- function(n, df, scale) {
    p <- nrow(scale)
    out <- array(NA_real_, c(p, p, n))
    inv_scale <- solve(scale)
    for (i in seq_len(n)) out[, , i] <- solve(rWishart(1, df, inv_scale)[, , 1])
    out
}

matrix_draws_to_rows <- function(a) {
    n <- dim(a)[3]
    t(vapply(seq_len(n), function(i) as.vector(a[, , i]), numeric(dim(a)[1] * dim(a)[2])))
}

numerical_posterior_moments <- function(log_posterior, lower, upper,
                                        n_grid = 12001L) {
    stopifnot(is.function(log_posterior), lower < upper, n_grid >= 1001L)
    
    grid <- seq(lower, upper, length.out = n_grid)
    log_post <- vapply(grid, log_posterior, numeric(1))
    finite <- is.finite(log_post)
    
    if (!any(finite)) {
        stop("Numerical posterior has no finite mass on the supplied grid")
    }
    
    log_post[!finite] <- -Inf
    log_post <- log_post - max(log_post)
    weights <- exp(log_post)
    
    # Trapezoidal-rule endpoint weights. The common grid spacing cancels
    # after normalization.
    weights[c(1L, length(weights))] <- weights[c(1L, length(weights))] / 2
    weights <- weights / sum(weights)
    
    post_mean <- sum(grid * weights)
    post_var <- sum((grid - post_mean)^2 * weights)
    
    c(mean = post_mean, sd = sqrt(post_var))
}

expect_numerical_posterior <- function(draws, parameter, log_posterior,
                                       lower, upper,
                                       mean_tolerance = 0.05,
                                       sd_tolerance = 0.01,
                                       n_grid = 12001L) {
    reference <- numerical_posterior_moments(
        log_posterior = log_posterior,
        lower = lower,
        upper = upper,
        n_grid = n_grid
    )
    
    sampled_mean <- mean(draws[, parameter])
    sampled_sd <- sd(draws[, parameter])
    reference_mean <- unname(reference["mean"])
    reference_sd <- unname(reference["sd"])
    
    # Treat the supplied tolerances as absolute Monte Carlo margins.
    # expect_equal() uses a scale-sensitive numerical comparison, which can
    # make very small posterior SD discrepancies fail unexpectedly.
    testthat::expect_lte(
        abs(sampled_mean - reference_mean),
        mean_tolerance,
        label = sprintf(
            "absolute posterior-mean error for %s (%.6f vs %.6f)",
            parameter, sampled_mean, reference_mean
        )
    )
    
    testthat::expect_lte(
        abs(sampled_sd - reference_sd),
        sd_tolerance,
        label = sprintf(
            "absolute posterior-SD error for %s (%.6f vs %.6f)",
            parameter, sampled_sd, reference_sd
        )
    )
    
    invisible(reference)
}

skip_if_reference_samplers_missing <- function() {
    testthat::skip_if_not_installed("rstan")
    testthat::skip_if_not_installed("rjags")
}

stan_test_draws <- function(model, data, parameters, seed = 4402L,
                            samples = 2000L, burnin = 1000L,
                            chains = 2L, verbose = FALSE) {
    skip_if_reference_samplers_missing()
    
    samples_per_chain <- ceiling(samples / chains)
    fit <- rstan::stan(
        model_code = model,
        data = data,
        pars = parameters,
        chains = chains,
        iter = burnin + samples_per_chain,
        warmup = burnin,
        seed = seed,
        refresh = 0,
        verbose = verbose,
        cores = 1,
        control = list(adapt_delta = 0.95)
    )
    
    out <- as.matrix(fit, pars = parameters)
    if (nrow(out) > samples) out <- out[seq_len(samples), , drop = FALSE]
    out
}

jags_test_draws <- function(model, data, parameters, seed = 4403L,
                            samples = 5000L, burnin = 1500L,
                            adapt = 500L, chains = 2L) {
    skip_if_reference_samplers_missing()
    
    rng_names <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry")
    inits <- lapply(seq_len(chains), function(i) {
        list(
            .RNG.name = rng_names[(i - 1L) %% length(rng_names) + 1L],
            .RNG.seed = as.integer(seed + i - 1L)
        )
    })
    
    con <- textConnection(model)
    on.exit(close(con), add = TRUE)
    
    fit <- rjags::jags.model(
        con,
        data = data,
        inits = inits,
        n.chains = chains,
        n.adapt = adapt,
        quiet = TRUE
    )
    stats::update(fit, n.iter = burnin, progress.bar = "none")
    
    samples_per_chain <- ceiling(samples / chains)
    draws <- rjags::coda.samples(
        fit,
        variable.names = parameters,
        n.iter = samples_per_chain,
        thin = 1L,
        progress.bar = "none"
    )
    
    out <- do.call(rbind, lapply(draws, as.matrix))
    if (nrow(out) > samples) out <- out[seq_len(samples), , drop = FALSE]
    out
}

posterior_draw_summary <- function(x) {
    qs <- stats::quantile(
        x,
        probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
        names = FALSE,
        type = 8
    )
    
    c(
        mean = mean(x),
        sd = stats::sd(x),
        q025 = qs[1L],
        q25 = qs[2L],
        median = qs[3L],
        q75 = qs[4L],
        q975 = qs[5L]
    )
}

posterior_wasserstein_1d <- function(x, y, n_quantiles = 1001L) {
    probs <- seq(0.001, 0.999, length.out = n_quantiles)
    qx <- stats::quantile(x, probs = probs, names = FALSE, type = 8)
    qy <- stats::quantile(y, probs = probs, names = FALSE, type = 8)
    mean(abs(qx - qy))
}

expect_posterior_matches_reference <- function(
        hobbs, stan, jags, parameter,
        mean_tolerance = 0.35,
        sd_log_tolerance = 0.25,
        quantile_tolerance = 0.55,
        wasserstein_tolerance = 0.35) {
    
    extract_parameter <- function(x, parameter) {
        testthat::expect_true(
            is.matrix(x) || is.data.frame(x),
            info = "Posterior draws must be a matrix or data frame"
        )
        
        parameter_base <- sub("\\[1\\]$", "", parameter)
        candidates <- unique(c(parameter, parameter_base))
        matched <- candidates[candidates %in% colnames(x)]
        
        testthat::expect_true(
            length(matched) > 0L,
            info = sprintf(
                "Posterior draws are missing parameter %s; available parameters: %s",
                parameter,
                paste(colnames(x), collapse = ", ")
            )
        )
        
        as.numeric(x[, matched[1L]])
    }
    
    matrices <- list(hobbs = hobbs, stan = stan, jags = jags)
    values <- lapply(matrices, extract_parameter, parameter = parameter)
    
    summaries <- lapply(values, posterior_draw_summary)
    summary_matrix <- do.call(rbind, summaries)
    
    pairs <- list(
        c("hobbs", "stan"),
        c("hobbs", "jags"),
        c("stan", "jags")
    )
    
    distances <- lapply(pairs, function(pair) {
        a_name <- pair[1L]
        b_name <- pair[2L]
        a <- values[[a_name]]
        b <- values[[b_name]]
        sa <- summaries[[a_name]]
        sb <- summaries[[b_name]]
        
        pooled_sd <- sqrt((sa["sd"]^2 + sb["sd"]^2) / 2)
        if (!is.finite(pooled_sd) || pooled_sd <= .Machine$double.eps) {
            pooled_sd <- max(abs(c(sa["mean"], sb["mean"])), 1)
        }
        
        mean_distance <- abs(sa["mean"] - sb["mean"]) / pooled_sd
        sd_log_distance <- abs(log(sa["sd"] / sb["sd"]))
        quantile_names <- c("q025", "q25", "median", "q75", "q975")
        quantile_distance <- max(abs(sa[quantile_names] - sb[quantile_names])) / pooled_sd
        wasserstein_distance <- posterior_wasserstein_1d(a, b) / pooled_sd
        
        testthat::expect_lte(
            unname(mean_distance),
            mean_tolerance,
            label = sprintf("standardized posterior-mean distance: %s vs %s", a_name, b_name)
        )
        testthat::expect_lte(
            unname(sd_log_distance),
            sd_log_tolerance,
            label = sprintf("posterior-SD log-ratio distance: %s vs %s", a_name, b_name)
        )
        testthat::expect_lte(
            unname(quantile_distance),
            quantile_tolerance,
            label = sprintf("maximum standardized posterior-quantile distance: %s vs %s", a_name, b_name)
        )
        testthat::expect_lte(
            unname(wasserstein_distance),
            wasserstein_tolerance,
            label = sprintf("standardized posterior Wasserstein distance: %s vs %s", a_name, b_name)
        )
        
        data.frame(
            sampler_a = a_name,
            sampler_b = b_name,
            mean_distance = unname(mean_distance),
            sd_log_distance = unname(sd_log_distance),
            quantile_distance = unname(quantile_distance),
            wasserstein_distance = unname(wasserstein_distance),
            stringsAsFactors = FALSE
        )
    })
    
    invisible(list(
        summaries = summary_matrix,
        distances = do.call(rbind, distances)
    ))
}

rows_to_matrix_array <- function(x, k) {
    n <- nrow(x)
    out <- array(NA_real_, dim = c(n, k, k))
    for (i in seq_len(n)) out[i, , ] <- matrix(x[i, ], k, k)
    out
}