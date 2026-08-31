test_that("dpois recovers lambda", {
  set.seed(125); truth <- 3.2; y <- rpois(80, truth)
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dpois(exp(eta(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "eta[1]"])), truth, tolerance = 0.45)
})

test_that("dpois gives the correct posterior for lambda", {
  set.seed(125)
  truth <- 3.2
  y <- rpois(80, truth)

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dpois(exp(eta(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(eta) {
    dnorm(eta, 0, 2, log = TRUE) +
      sum(dpois(y, lambda = exp(eta), log = TRUE))
  }

  expect_numerical_posterior(
    d, "eta[1]", log_posterior,
    lower = -1, upper = 2.5,
    mean_tolerance = 0.035, sd_tolerance = 0.025
  )
})

