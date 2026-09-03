skip_if_hobbs_toolchain_missing <- function() {
  testthat::skip_if(!nzchar(Sys.which("cargo")), "Cargo is required for hobbs integration tests")
  testthat::skip_if(!nzchar(Sys.which("rustc")), "rustc is required for hobbs integration tests")
  cc <- c(Sys.which("cc"), Sys.which("clang"), Sys.which("gcc"))
  testthat::skip_if(!any(nzchar(cc)), "A C compiler is required for hobbs integration tests")
}

hobbs_test_draws <- function(model, data = list(), seed = 4401L,
                             samples = 3000L, burnin = 1500L) {
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
