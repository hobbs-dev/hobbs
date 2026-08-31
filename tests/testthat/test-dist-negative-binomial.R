test_that("dnbinom recovers probability", {
  set.seed(127); truth <- 0.58; y <- rnbinom(100, size = 4, prob = truth)
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dnbinom(4, inv_logit(eta(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(inv_logit_r(mean(d[, "eta[1]"])), truth, tolerance = 0.09)
})

test_that("dnbinom gives the correct posterior for probability", {
  set.seed(127)
  truth <- 0.58
  size <- 4
  y <- rnbinom(100, size = size, prob = truth)

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dnbinom(4, inv_logit(eta(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(eta) {
    dnorm(eta, 0, 2, log = TRUE) +
      sum(dnbinom(y, size = size, prob = plogis(eta), log = TRUE))
  }

  expect_numerical_posterior(
    d, "eta[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

