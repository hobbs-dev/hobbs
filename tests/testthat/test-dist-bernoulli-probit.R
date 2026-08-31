test_that("bernoulli_probit recovers eta", {
  set.seed(121); truth <- 0.6; y <- rbinom(120, 1, pnorm(truth))
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ bernoulli_probit(eta(1));
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "eta[1]", truth, 0.35)
})

test_that("bernoulli_probit gives the correct posterior", {
  set.seed(121)
  truth <- 0.6
  y <- rbinom(120, 1, pnorm(truth))

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ bernoulli_probit(eta(1));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(eta) {
    dnorm(eta, 0, 2, log = TRUE) +
      sum(dbinom(y, 1, pnorm(eta), log = TRUE))
  }

  expect_numerical_posterior(
    d, "eta[1]", log_posterior,
    lower = -2.5, upper = 2.5,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

