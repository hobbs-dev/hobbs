test_that("dlkjcorr2 recovers eta", {
  set.seed(133); n <- 80; truth <- 2.2; rho <- 2 * rbeta(n, truth, truth) - 1
  Rdat <- cbind(1, rho, rho, 1)
  model <- 'param log_eta(1);
block log_eta(1) {
log_eta(1) ~ dnorm(0, 2);
for (i = 1:n) Rdat(i, 1:4) ~ dlkjcorr2(exp(log_eta(1)));
}'
  d <- hobbs_test_draws(model, list(Rdat = Rdat)); testthat::expect_equal(exp(mean(d[, "log_eta[1]"])), truth, tolerance = 0.65)
})

test_that("dlkjcorr2 gives the correct posterior for eta", {
  set.seed(133)
  n <- 80
  truth <- 2.2
  rho <- 2 * rbeta(n, truth, truth) - 1
  Rdat <- cbind(1, rho, rho, 1)

  model <- 'param log_eta(1);
block log_eta(1) {
log_eta(1) ~ dnorm(0, 2);
for (i = 1:n) Rdat(i, 1:4) ~ dlkjcorr2(exp(log_eta(1)));
}'

  d <- hobbs_test_draws(model, list(Rdat = Rdat))

  log_one_minus_rho2 <- sum(log1p(-rho^2))
  log_posterior <- function(log_eta) {
    eta <- exp(log_eta)
    log_norm <- lgamma(eta + 0.5) - 0.5 * log(pi) - lgamma(eta)
    dnorm(log_eta, 0, 2, log = TRUE) +
      n * log_norm +
      (eta - 1) * log_one_minus_rho2
  }

  expect_numerical_posterior(
    d, "log_eta[1]", log_posterior,
    lower = -1.5, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

test_that("dlkjcorr2 matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(133)
  n <- 80
  truth <- 2.2
  rho <- 2 * rbeta(n, truth, truth) - 1
  u <- (rho + 1) / 2
  Rdat <- cbind(1, rho, rho, 1)

  hobbs_model <- 'param log_eta(1);
block log_eta(1) {
log_eta(1) ~ dnorm(0, 2);
for (i = 1:n) Rdat(i, 1:4) ~ dlkjcorr2(exp(log_eta(1)));
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0, upper=1>[n] u;
}
parameters {
  vector[1] log_eta;
}
model {
  log_eta[1] ~ normal(0, 2);
  u ~ beta(exp(log_eta[1]), exp(log_eta[1]));
}'

  jags_model <- '
model {
  log_eta[1] ~ dnorm(0, 0.25)
  eta <- exp(log_eta[1])
  for (i in 1:n) u[i] ~ dbeta(eta, eta)
}'

  d_hobbs <- hobbs_test_draws(hobbs_model, list(Rdat = Rdat))
  d_stan <- stan_test_draws(stan_model, list(n = n, u = u), "log_eta")
  d_jags <- jags_test_draws(jags_model, list(n = n, u = u), "log_eta")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_eta[1]")
})

