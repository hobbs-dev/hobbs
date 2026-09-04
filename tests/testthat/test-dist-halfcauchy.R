test_that("dhalfcauchy recovers its scale", {
  set.seed(118); truth <- 1.2; y <- r_halfcauchy(100, truth)
  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dhalfcauchy(exp(log_scale(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "log_scale[1]"])), truth, tolerance = 0.40)
})

test_that("dhalfcauchy gives the correct posterior for its scale", {
  set.seed(118)
  truth <- 1.2
  y <- r_halfcauchy(100, truth)

  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dhalfcauchy(exp(log_scale(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(log_scale) {
    scale <- exp(log_scale)
    # The half-Cauchy factor of 2 is constant in log_scale and cancels.
    dnorm(log_scale, 0, 2, log = TRUE) +
      sum(dcauchy(y, 0, scale, log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_scale[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

test_that("dhalfcauchy matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(118)
  truth <- 1.2
  y <- r_halfcauchy(100, truth)
  n <- length(y)

  hobbs_model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dhalfcauchy(exp(log_scale(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0>[n] y;
}
parameters {
  vector[1] log_scale;
}
model {
  log_scale[1] ~ normal(0, 2);
  y ~ cauchy(0, exp(log_scale[1]));
}'

  jags_model <- '
model {
  log_scale[1] ~ dnorm(0, 0.25)
  tau <- exp(-2 * log_scale[1])
  for (i in 1:n) y[i] ~ dt(0, tau, 1)
}'

  data <- list(n = n, y = y)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "log_scale")
  d_jags <- jags_test_draws(jags_model, data, "log_scale")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_scale[1]")
})

