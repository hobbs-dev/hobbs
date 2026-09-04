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

test_that("dnbinom_log matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(128)
  truth <- 0.8
  size <- 4L
  y <- rnbinom(100, size = size, mu = exp(truth))
  n <- length(y)

  hobbs_model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dnbinom_log(eta(1), 4);
}'

  stan_model <- '
data {
  int<lower=1> n;
  int<lower=1> size;
  array[n] int<lower=0> y;
}
parameters {
  vector[1] eta;
}
model {
  eta[1] ~ normal(0, 2);
  for (i in 1:n)
    y[i] ~ neg_binomial_2_log(eta[1], size);
}'

  jags_model <- '
model {
  eta[1] ~ dnorm(0, 0.25)
  mu <- exp(eta[1])
  p <- size / (size + mu)
  for (i in 1:n) y[i] ~ dnegbin(p, size)
}'

  data <- list(n = n, y = y, size = size)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "eta")
  d_jags <- jags_test_draws(jags_model, data, "eta")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "eta[1]")
})

