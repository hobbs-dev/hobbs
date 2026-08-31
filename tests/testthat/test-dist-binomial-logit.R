test_that("binomial_logit recovers eta", {
  set.seed(124); m <- 8L; truth <- 0.55; y <- rbinom(70, m, inv_logit_r(truth))
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ binomial_logit(m, eta(1));
}'
  d <- hobbs_test_draws(model, list(y = y, m = m)); expect_posterior_near(d, "eta[1]", truth, 0.20)
})

test_that("binomial_logit gives the correct posterior", {
  set.seed(124)
  m <- 8L
  truth <- 0.55
  y <- rbinom(70, m, inv_logit_r(truth))

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ binomial_logit(m, eta(1));
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

