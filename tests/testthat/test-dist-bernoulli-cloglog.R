test_that("bernoulli_cloglog recovers eta", {
  set.seed(122); truth <- -0.35; y <- rbinom(120, 1, inv_cloglog_r(truth))
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ bernoulli_cloglog(eta(1));
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "eta[1]", truth, 0.35)
})

test_that("bernoulli_cloglog gives the correct posterior", {
  set.seed(122)
  truth <- -0.35
  y <- rbinom(120, 1, inv_cloglog_r(truth))

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ bernoulli_cloglog(eta(1));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(eta) {
    p <- inv_cloglog_r(eta)
    dnorm(eta, 0, 2, log = TRUE) +
      sum(dbinom(y, 1, p, log = TRUE))
  }

  expect_numerical_posterior(
    d, "eta[1]", log_posterior,
    lower = -3, upper = 2,
    mean_tolerance = 0.04
  )
})

