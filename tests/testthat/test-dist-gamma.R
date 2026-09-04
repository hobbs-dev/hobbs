test_that("dgamma recovers its rate", {
  set.seed(106); shape <- 2.5; truth <- 1.4; y <- rgamma(70, shape, rate = truth)
  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dgamma(shape, exp(log_rate(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, shape = shape)); testthat::expect_equal(exp(mean(d[, "log_rate[1]"])), truth, tolerance = 0.30)
})

test_that("dgamma gives the correct posterior for its rate", {
  set.seed(106)
  shape <- 2.5
  truth <- 1.4
  y <- rgamma(70, shape, rate = truth)

  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dgamma(shape, exp(log_rate(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, shape = shape))

  log_posterior <- function(log_rate) {
    dnorm(log_rate, 0, 2, log = TRUE) +
      sum(dgamma(y, shape = shape, rate = exp(log_rate), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_rate[1]", log_posterior,
    lower = -1, upper = 2,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

test_that("dgamma matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(106)
  shape <- 2.5
  truth <- 1.4
  y <- rgamma(70, shape, rate = truth)
  n <- length(y)

  hobbs_model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dgamma(shape, exp(log_rate(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0>[n] y;
  real<lower=0> shape;
}
parameters {
  vector[1] log_rate;
}
model {
  log_rate[1] ~ normal(0, 2);
  y ~ gamma(shape, exp(log_rate[1]));
}'

  jags_model <- '
model {
  log_rate[1] ~ dnorm(0, 0.25)
  for (i in 1:n) y[i] ~ dgamma(shape, exp(log_rate[1]))
}'

  data <- list(n = n, y = y, shape = shape)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y, shape = shape))
  d_stan <- stan_test_draws(stan_model, data, "log_rate")
  d_jags <- jags_test_draws(jags_model, data, "log_rate")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_rate[1]")
})

