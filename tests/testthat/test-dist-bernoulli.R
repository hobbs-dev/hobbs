test_that("dbern recovers probability", {
  set.seed(119); truth <- 0.68; y <- rbinom(100, 1, truth)
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbern(inv_logit(eta(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(inv_logit_r(mean(d[, "eta[1]"])), truth, tolerance = 0.12)
})

test_that("dbern gives the correct posterior", {
  set.seed(119)
  truth <- 0.68
  y <- rbinom(100, 1, truth)

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbern(inv_logit(eta(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(eta) {
    dnorm(eta, 0, 2, log = TRUE) +
      sum(dbinom(y, 1, plogis(eta), log = TRUE))
  }

  expect_numerical_posterior(
    d, "eta[1]", log_posterior,
    lower = -2.5, upper = 3,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

