test_that("dlogis recovers its location", {
  set.seed(113); truth <- 0.7; y <- rlogis(80, truth, 0.8)
  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlogis(loc(1), 0.8);
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "loc[1]", truth, 0.30)
})

test_that("dlogis gives the correct posterior for its location", {
  set.seed(113)
  truth <- 0.7
  scale <- 0.8
  y <- rlogis(80, truth, scale)

  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlogis(loc(1), 0.8);
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(loc) {
    dnorm(loc, 0, 3, log = TRUE) +
      sum(dlogis(y, loc, scale, log = TRUE))
  }

  expect_numerical_posterior(
    d, "loc[1]", log_posterior,
    lower = -2, upper = 3,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

test_that("dlogis matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(113)
  truth <- 0.7
  scale <- 0.8
  y <- rlogis(80, truth, scale)
  n <- length(y)
  hobbs_model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlogis(loc(1), 0.8);
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector[n] y;
  real<lower=0> scale;
}
parameters {
  vector[1] loc;
}
model {
  loc[1] ~ normal(0, 3);
  y ~ logistic(loc[1], scale);
}'

  jags_model <- '
model {
  loc[1] ~ dnorm(0, 0.1111111111111111)
  for (i in 1:n) y[i] ~ dlogis(loc[1], 1 / scale)
}'

  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(
    stan_model,
    list(n = n, y = y, scale = scale),
    "loc"
  )
  d_jags <- jags_test_draws(
    jags_model,
    list(n = n, y = y, scale = scale),
    "loc"
  )

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "loc[1]")
})

