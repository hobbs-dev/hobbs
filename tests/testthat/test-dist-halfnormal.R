test_that("dhalfnorm recovers its scale", {
  set.seed(117); truth <- 1.3; y <- r_halfnormal(80, truth)
  model <- 'param log_sd(1);
block log_sd(1) {
log_sd(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dhalfnorm(exp(log_sd(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "log_sd[1]"])), truth, tolerance = 0.28)
})

test_that("dhalfnorm gives the correct posterior for its scale", {
  set.seed(117)
  truth <- 1.3
  y <- r_halfnormal(80, truth)

  model <- 'param log_sd(1);
block log_sd(1) {
log_sd(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dhalfnorm(exp(log_sd(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(log_sd) {
    sigma <- exp(log_sd)
    # The half-normal factor of 2 is constant in sigma and cancels.
    dnorm(log_sd, 0, 2, log = TRUE) +
      sum(dnorm(y, 0, sigma, log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_sd[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.04
  )
})

