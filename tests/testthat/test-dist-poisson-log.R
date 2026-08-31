test_that("poisson_log recovers eta", {
  set.seed(126); truth <- 1.0; y <- rpois(80, exp(truth))
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ poisson_log(eta(1));
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "eta[1]", truth, 0.18)
})

test_that("poisson_log gives the correct posterior", {
  set.seed(126)
  truth <- 1.0
  y <- rpois(80, exp(truth))

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ poisson_log(eta(1));
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

