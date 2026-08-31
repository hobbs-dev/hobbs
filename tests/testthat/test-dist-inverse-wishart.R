test_that("dinvwish recovers an isotropic scale", {
  set.seed(132); n <- 35; k <- 2L; df <- 7; truth <- 1.4; S <- diag(truth, k); W <- matrix_draws_to_rows(r_invwishart(n, df, S))
  model <- 'param log_scale(1);
block log_scale(1) {
double s = exp(log_scale(1));
double S[4] = {s, 0.0, 0.0, s};
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) W(i, 1:4) ~ dinvwish(S, df, 2);
}'
  d <- hobbs_test_draws(model, list(W = W, df = df)); testthat::expect_equal(exp(mean(d[, "log_scale[1]"])), truth, tolerance = 0.38)
})

test_that("dinvwish gives the correct posterior for an isotropic scale", {
  set.seed(132)
  n <- 35
  k <- 2L
  df <- 7
  truth <- 1.4
  S <- diag(truth, k)
  W <- matrix_draws_to_rows(r_invwishart(n, df, S))

  model <- 'param log_scale(1);
block log_scale(1) {
double s = exp(log_scale(1));
double S[4] = {s, 0.0, 0.0, s};
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) W(i, 1:4) ~ dinvwish(S, df, 2);
}'

  d <- hobbs_test_draws(model, list(W = W, df = df))

  trace_inv_sum <- sum(vapply(seq_len(n), function(i) {
    Wi <- matrix(W[i, ], k, k)
    sum(diag(solve(Wi)))
  }, numeric(1)))

  log_posterior <- function(log_scale) {
    s <- exp(log_scale)
    dnorm(log_scale, 0, 2, log = TRUE) +
      0.5 * n * df * k * log_scale -
      0.5 * s * trace_inv_sum
  }

  expect_numerical_posterior(
    d, "log_scale[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

