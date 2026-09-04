test_that("dbeta recovers its first shape parameter", {
  set.seed(108); truth <- 2.2; b <- 3.0; y <- rbeta(80, truth, b)
  model <- 'param log_a(1);
block log_a(1) {
log_a(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbeta(exp(log_a(1)), b);
}'
  d <- hobbs_test_draws(model, list(y = y, b = b)); testthat::expect_equal(exp(mean(d[, "log_a[1]"])), truth, tolerance = 0.55)
})

test_that("dbeta gives the correct posterior for its first shape parameter", {
  set.seed(108)
  truth <- 2.2
  b <- 3.0
  y <- rbeta(80, truth, b)

  model <- 'param log_a(1);
block log_a(1) {
log_a(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbeta(exp(log_a(1)), b);
}'

  d <- hobbs_test_draws(model, list(y = y, b = b))

  log_posterior <- function(log_a) {
    dnorm(log_a, 0, 2, log = TRUE) +
      sum(dbeta(y, exp(log_a), b, log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_a[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

test_that("dbeta matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(108)
  truth <- 2.2
  b <- 3.0
  y <- rbeta(80, truth, b)
  n <- length(y)

  hobbs_model <- 'param log_a(1);
block log_a(1) {
log_a(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbeta(exp(log_a(1)), b);
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0, upper=1>[n] y;
  real<lower=0> b;
}
parameters {
  vector[1] log_a;
}
model {
  log_a[1] ~ normal(0, 2);
  y ~ beta(exp(log_a[1]), b);
}'

  jags_model <- '
model {
  log_a[1] ~ dnorm(0, 0.25)
  for (i in 1:n) y[i] ~ dbeta(exp(log_a[1]), b)
}'

  data <- list(n = n, y = y, b = b)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y, b = b))
  d_stan <- stan_test_draws(stan_model, data, "log_a")
  d_jags <- jags_test_draws(jags_model, data, "log_a")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_a[1]")
})

