test_that("dt recovers its location", {
  set.seed(110); truth <- -0.6; df <- 5; y <- truth + rt(80, df)
  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dt(df, loc(1), 1.0);
}'
  d <- hobbs_test_draws(model, list(y = y, df = df)); expect_posterior_near(d, "loc[1]", truth, 0.32)
})

test_that("dt gives the correct posterior for its location", {
  set.seed(110)
  truth <- -0.6
  df <- 5
  y <- truth + rt(80, df)

  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dt(df, loc(1), 1.0);
}'

  d <- hobbs_test_draws(model, list(y = y, df = df))

  log_posterior <- function(loc) {
    dnorm(loc, 0, 3, log = TRUE) +
      sum(dt(y - loc, df = df, log = TRUE))
  }

  expect_numerical_posterior(
    d, "loc[1]", log_posterior,
    lower = -3, upper = 2,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

test_that("dt matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(110)
  truth <- -0.6
  df <- 5
  y <- truth + rt(80, df)
  n <- length(y)

  hobbs_model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dt(df, loc(1), 1.0);
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector[n] y;
  real<lower=0> df;
}
parameters {
  vector[1] loc;
}
model {
  loc[1] ~ normal(0, 3);
  y ~ student_t(df, loc[1], 1);
}'

  jags_model <- '
model {
  loc[1] ~ dnorm(0, 0.1111111111111111)
  for (i in 1:n) y[i] ~ dt(loc[1], 1, df)
}'

  data <- list(n = n, y = y, df = df)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y, df = df))
  d_stan <- stan_test_draws(stan_model, data, "loc")
  d_jags <- jags_test_draws(jags_model, data, "loc")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "loc[1]")
})

