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

