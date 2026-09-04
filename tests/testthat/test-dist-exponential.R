test_that("dexp recovers its rate", {
  set.seed(105); truth <- 1.7; y <- rexp(70, truth)
  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dexp(exp(log_rate(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "log_rate[1]"])), truth, tolerance = 0.38)
})

test_that("dexp gives the correct posterior for its rate", {
  set.seed(105)
  truth <- 1.7
  y <- rexp(70, truth)

  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dexp(exp(log_rate(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(log_rate) {
    dnorm(log_rate, 0, 2, log = TRUE) +
      sum(dexp(y, rate = exp(log_rate), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_rate[1]", log_posterior,
    lower = -1.5, upper = 2,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

test_that("dexp matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(105)
  truth <- 1.7
  y <- rexp(70, truth)
  n <- length(y)

  hobbs_model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dexp(exp(log_rate(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0>[n] y;
}
parameters {
  vector[1] log_rate;
}
model {
  log_rate[1] ~ normal(0, 2);
  y ~ exponential(exp(log_rate[1]));
}'

  jags_model <- '
model {
  log_rate[1] ~ dnorm(0, 0.25)
  for (i in 1:n) y[i] ~ dexp(exp(log_rate[1]))
}'

  data <- list(n = n, y = y)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "log_rate")
  d_jags <- jags_test_draws(jags_model, data, "log_rate")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_rate[1]")
})

