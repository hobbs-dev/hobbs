test_that("bernoulli_logit recovers eta", {
  set.seed(120); truth <- 0.75; y <- rbinom(100, 1, inv_logit_r(truth))
  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ bernoulli_logit(eta(1));
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "eta[1]", truth, 0.45)
})

test_that("bernoulli_logit gives the correct posterior", {
  set.seed(120)
  truth <- 0.75
  y <- rbinom(100, 1, inv_logit_r(truth))

  model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ bernoulli_logit(eta(1));
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

test_that("bernoulli_logit matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(120)
  truth <- 0.75
  y <- rbinom(100, 1, inv_logit_r(truth))
  n <- length(y)

  hobbs_model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ bernoulli_logit(eta(1));
}'

  stan_model <- '
data {
  int<lower=1> n;
  array[n] int<lower=0, upper=1> y;
}
parameters {
  vector[1] eta;
}
model {
  eta[1] ~ normal(0, 2);
  for (i in 1:n) y[i] ~ bernoulli_logit(eta[1]);
}'

jags_model <- '
model {
  eta[1] ~ dnorm(0, 0.25)
  p <- ilogit(eta[1])

  for (i in 1:n) {
    y[i] ~ dbern(p)
  }
}'

  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, list(n = n, y = y), "eta")
  d_jags <- jags_test_draws(jags_model, list(n = n, y = y), "eta")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "eta[1]")
})

