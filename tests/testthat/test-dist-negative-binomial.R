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

test_that("dnbinom matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(127)
  truth <- 0.58
  size <- 4L
  y <- rnbinom(100, size = size, prob = truth)
  n <- length(y)

  hobbs_model <- 'param eta(1);
block eta(1) {
eta(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dnbinom(4, inv_logit(eta(1)));
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
transformed parameters {
  real<lower=0, upper=1> p;
  p = inv_logit(eta[1]);
}
model {
  eta[1] ~ normal(0, 2);
  for (i in 1:n)
    y[i] ~ neg_binomial(size, p / (1 - p));
}'

  jags_model <- '
model {
  eta[1] ~ dnorm(0, 0.25)
  p <- ilogit(eta[1])
  for (i in 1:n) y[i] ~ dnegbin(p, size)
}'

  data <- list(n = n, y = y, size = size)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "eta")
  d_jags <- jags_test_draws(jags_model, data, "eta")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "eta[1]")
})

