test_that("dchisq recovers degrees of freedom", {
  set.seed(111); truth <- 4.5; y <- rchisq(100, truth)
  model <- 'param log_df(1);
block log_df(1) {
log_df(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dchisq(exp(log_df(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "log_df[1]"])), truth, tolerance = 0.70)
})

test_that("dchisq gives the correct posterior for degrees of freedom", {
  set.seed(111)
  truth <- 4.5
  y <- rchisq(100, truth)

  model <- 'param log_df(1);
block log_df(1) {
log_df(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dchisq(exp(log_df(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(log_df) {
    dnorm(log_df, 1, 2, log = TRUE) +
      sum(dchisq(y, df = exp(log_df), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_df[1]", log_posterior,
    lower = -0.5, upper = 3,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

test_that("dchisq matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(111)
  truth <- 4.5
  y <- rchisq(100, truth)
  n <- length(y)

  hobbs_model <- 'param log_df(1);
block log_df(1) {
log_df(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dchisq(exp(log_df(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0>[n] y;
}
parameters {
  vector[1] log_df;
}
model {
  log_df[1] ~ normal(1, 2);
  for (i in 1:n) y[i] ~ chi_square(exp(log_df[1]));
}'

  jags_model <- '
model {
  log_df[1] ~ dnorm(1, 0.25)
  for (i in 1:n) y[i] ~ dchisqr(exp(log_df[1]))
}'

  data <- list(n = n, y = y)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "log_df")
  d_jags <- jags_test_draws(jags_model, data, "log_df")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_df[1]")
})

