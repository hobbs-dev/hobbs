test_that("dhalfcauchy recovers its scale", {
  set.seed(118); truth <- 1.2; y <- r_halfcauchy(100, truth)
  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dhalfcauchy(exp(log_scale(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "log_scale[1]"])), truth, tolerance = 0.40)
})

test_that("dhalfcauchy gives the correct posterior for its scale", {
  set.seed(118)
  truth <- 1.2
  y <- r_halfcauchy(100, truth)

  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dhalfcauchy(exp(log_scale(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(log_scale) {
    scale <- exp(log_scale)
    # The half-Cauchy factor of 2 is constant in log_scale and cancels.
    dnorm(log_scale, 0, 2, log = TRUE) +
      sum(dcauchy(y, 0, scale, log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_scale[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

