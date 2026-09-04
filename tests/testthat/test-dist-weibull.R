test_that("dweibull recovers its scale", {
  set.seed(115); shape <- 1.8; truth <- 1.4; y <- rweibull(80, shape, truth)
  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dweibull(shape, exp(log_scale(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, shape = shape)); testthat::expect_equal(exp(mean(d[, "log_scale[1]"])), truth, tolerance = 0.28)
})

test_that("dweibull gives the correct posterior for its scale", {
  set.seed(115)
  shape <- 1.8
  truth <- 1.4
  y <- rweibull(80, shape, truth)

  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dweibull(shape, exp(log_scale(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, shape = shape))

  log_posterior <- function(log_scale) {
    dnorm(log_scale, 0, 2, log = TRUE) +
      sum(dweibull(y, shape = shape, scale = exp(log_scale), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_scale[1]", log_posterior,
    lower = -1.5, upper = 2,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

test_that("dweibull matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(115)
  shape <- 1.8
  truth <- 1.4
  y <- rweibull(80, shape, truth)
  n <- length(y)

  hobbs_model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dweibull(shape, exp(log_scale(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0>[n] y;
  real<lower=0> shape;
}
parameters {
  vector[1] log_scale;
}
model {
  log_scale[1] ~ normal(0, 2);
  y ~ weibull(shape, exp(log_scale[1]));
}'

  jags_model <- '
model {
  log_scale[1] ~ dnorm(0, 0.25)
  scale <- exp(log_scale[1])
  lambda <- pow(scale, -shape)
  for (i in 1:n) y[i] ~ dweib(shape, lambda)
}'

  data <- list(n = n, y = y, shape = shape)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y, shape = shape))
  d_stan <- stan_test_draws(stan_model, data, "log_scale")
  d_jags <- jags_test_draws(jags_model, data, "log_scale")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_scale[1]")
})

