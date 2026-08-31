test_that("dnbinom_log recovers log mean", {
  set.seed(128); truth <- 0.8; mu <- exp(truth); y <- rnbinom(100, size = 4, mu = mu)
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dnbinom_log(eta(1), 4);
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "eta[1]", truth, 0.25)
})

test_that("dnbinom_log gives the correct posterior for log mean", {
  set.seed(128)
  truth <- 0.8
  size <- 4
  y <- rnbinom(100, size = size, mu = exp(truth))

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dnbinom_log(eta(1), 4);
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(eta) {
    dnorm(eta, 0, 2, log = TRUE) +
      sum(dnbinom(y, size = size, mu = exp(eta), log = TRUE))
  }

  expect_numerical_posterior(
    d, "eta[1]", log_posterior,
    lower = -1.5, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

