test_that("dcauchy recovers its location", {
  set.seed(109); truth <- 0.65; y <- rcauchy(90, truth, 1)
  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dcauchy(loc(1), 1.0);
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "loc[1]", truth, 0.38)
})

test_that("dcauchy gives the correct posterior for its location", {
  set.seed(109)
  truth <- 0.65
  y <- rcauchy(90, truth, 1)

  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dcauchy(loc(1), 1.0);
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(loc) {
    dnorm(loc, 0, 3, log = TRUE) +
      sum(dcauchy(y, loc, 1, log = TRUE))
  }

  expect_numerical_posterior(
    d, "loc[1]", log_posterior,
    lower = -2.5, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

test_that("dcauchy matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(109)
  truth <- 0.65
  y <- rcauchy(90, truth, 1)
  n <- length(y)

  hobbs_model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dcauchy(loc(1), 1.0);
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector[n] y;
}
parameters {
  vector[1] loc;
}
model {
  loc[1] ~ normal(0, 3);
  y ~ cauchy(loc[1], 1);
}'

  jags_model <- '
model {
  loc[1] ~ dnorm(0, 0.1111111111111111)
  for (i in 1:n) y[i] ~ dt(loc[1], 1, 1)
}'

  data <- list(n = n, y = y)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "loc")
  d_jags <- jags_test_draws(jags_model, data, "loc")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "loc[1]")
})

