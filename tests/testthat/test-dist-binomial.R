test_that("dbinom recovers probability", {
  set.seed(123); m <- 8L; truth <- 0.62; y <- rbinom(70, m, truth)
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbinom(m, inv_logit(eta(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, m = m)); testthat::expect_equal(inv_logit_r(mean(d[, "eta[1]"])), truth, tolerance = 0.07)
})

test_that("dbinom gives the correct posterior", {
  set.seed(123)
  m <- 8L
  truth <- 0.62
  y <- rbinom(70, m, truth)

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbinom(m, inv_logit(eta(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, m = m))

  log_posterior <- function(eta) {
    dnorm(eta, 0, 2, log = TRUE) +
      sum(dbinom(y, m, plogis(eta), log = TRUE))
  }

  expect_numerical_posterior(
    d, "eta[1]", log_posterior,
    lower = -1.5, upper = 2.5,
    mean_tolerance = 0.035, sd_tolerance = 0.025
  )
})

