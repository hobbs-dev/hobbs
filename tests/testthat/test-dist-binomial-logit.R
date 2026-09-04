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

test_that("binomial_logit matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(124)
  m <- 8L
  truth <- 0.55
  y <- rbinom(70, m, inv_logit_r(truth))
  n <- length(y)

  hobbs_model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ binomial_logit(m, eta(1));
}'

  stan_model <- '
data {
  int<lower=1> n;
  int<lower=1> m;
  array[n] int<lower=0> y;
}
parameters {
  vector[1] eta;
}
model {
  eta[1] ~ normal(0, 2);
  for (i in 1:n) y[i] ~ binomial_logit(m, eta[1]);
}'

  jags_model <- '
model {
  eta[1] ~ dnorm(0, 0.25)
  p <- ilogit(eta[1])
  for (i in 1:n) y[i] ~ dbin(p, m)
}'

  data <- list(n = n, y = y, m = m)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y, m = m))
  d_stan <- stan_test_draws(stan_model, data, "eta")
  d_jags <- jags_test_draws(jags_model, data, "eta")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "eta[1]")
})

