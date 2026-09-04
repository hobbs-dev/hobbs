test_that("dpareto recovers alpha", {
  set.seed(116); xmin <- 1; truth <- 3.2; y <- r_pareto(100, xmin, truth)
  model <- 'param log_alpha(1);
block log_alpha(1) {
log_alpha(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dpareto(xmin, exp(log_alpha(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, xmin = xmin)); testthat::expect_equal(exp(mean(d[, "log_alpha[1]"])), truth, tolerance = 0.70)
})

test_that("dpareto gives the correct posterior for alpha", {
  set.seed(116)
  xmin <- 1
  truth <- 3.2
  y <- r_pareto(100, xmin, truth)

  model <- 'param log_alpha(1);
block log_alpha(1) {
log_alpha(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dpareto(xmin, exp(log_alpha(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, xmin = xmin))

  log_posterior <- function(log_alpha) {
    alpha <- exp(log_alpha)
    dnorm(log_alpha, 1, 2, log = TRUE) +
      length(y) * log(alpha) +
      length(y) * alpha * log(xmin) -
      (alpha + 1) * sum(log(y))
  }

  expect_numerical_posterior(
    d, "log_alpha[1]", log_posterior,
    lower = -0.5, upper = 2.5,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

test_that("dpareto matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(116)
  xmin <- 1
  truth <- 3.2
  y <- r_pareto(100, xmin, truth)
  n <- length(y)
  hobbs_model <- 'param log_alpha(1);
block log_alpha(1) {
log_alpha(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dpareto(xmin, exp(log_alpha(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0>[n] y;
  real<lower=0> xmin;
}
parameters {
  vector[1] log_alpha;
}
model {
  log_alpha[1] ~ normal(1, 2);
  y ~ pareto(xmin, exp(log_alpha[1]));
}'

  jags_model <- '
model {
  log_alpha[1] ~ dnorm(1, 0.25)
  alpha <- exp(log_alpha[1])
  for (i in 1:n) y[i] ~ dpar(alpha, xmin)
}'

  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y, xmin = xmin))
  d_stan <- stan_test_draws(
    stan_model,
    list(n = n, y = y, xmin = xmin),
    "log_alpha"
  )
  d_jags <- jags_test_draws(
    jags_model,
    list(n = n, y = y, xmin = xmin),
    "log_alpha"
  )

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_alpha[1]")
})

