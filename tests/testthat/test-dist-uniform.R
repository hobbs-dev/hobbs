test_that("dunif recovers a centered uniform location", {
  set.seed(104); truth <- 0; y <- runif(80, truth - 1, truth + 1)
  model <- 'param mu(1);
block mu(1) {
mu(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dunif(mu(1) - 1.0, mu(1) + 1.0);
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "mu[1]", truth, 0.12)
})

test_that("dunif gives the correct posterior for a centered location", {
  set.seed(104)
  truth <- 0
  y <- runif(80, truth - 1, truth + 1)

  model <- 'param mu(1);
block mu(1) {
mu(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dunif(mu(1) - 1.0, mu(1) + 1.0);
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(mu) {
    if (any(y < mu - 1) || any(y > mu + 1)) return(-Inf)
    dnorm(mu, 0, 3, log = TRUE)
  }

  expect_numerical_posterior(
    d, "mu[1]", log_posterior,
    lower = -0.5, upper = 0.5,
    mean_tolerance = 0.03, sd_tolerance = 0.02,
    n_grid = 20001L
  )
})

test_that("dunif matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(104)
  truth <- 0
  y <- runif(80, truth - 1, truth + 1)
  lower_mu <- max(y) - 1
  upper_mu <- min(y) + 1

  hobbs_model <- 'param mu(1);
block mu(1) {
mu(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dunif(mu(1) - 1.0, mu(1) + 1.0);
}'

  stan_model <- '
data {
  real lower_mu;
  real upper_mu;
}
parameters {
  vector<lower=lower_mu, upper=upper_mu>[1] mu;
}
model {
  mu[1] ~ normal(0, 3);
}'

  jags_model <- '
model {
  mu[1] ~ dnorm(0, 0.1111111111111111) T(lower_mu, upper_mu)
}'

  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(
    stan_model,
    list(lower_mu = lower_mu, upper_mu = upper_mu),
    "mu"
  )
  d_jags <- jags_test_draws(
    jags_model,
    list(lower_mu = lower_mu, upper_mu = upper_mu),
    "mu"
  )

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "mu[1]")
})

