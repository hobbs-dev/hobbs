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

test_that("dpois matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(125)
  truth <- 3.2
  y <- rpois(80, truth)
  n <- length(y)

  hobbs_model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dpois(exp(eta(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  array[n] int<lower=0> y;
}
parameters {
  vector[1] eta;
}
model {
  eta[1] ~ normal(0, 2);
  for (i in 1:n) y[i] ~ poisson(exp(eta[1]));
}'

  jags_model <- '
model {
  eta[1] ~ dnorm(0, 0.25)
  lambda <- exp(eta[1])
  for (i in 1:n) y[i] ~ dpois(lambda)
}'

  data <- list(n = n, y = y)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "eta")
  d_jags <- jags_test_draws(jags_model, data, "eta")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "eta[1]")
})

