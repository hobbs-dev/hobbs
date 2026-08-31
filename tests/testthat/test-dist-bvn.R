test_that("dbvn recovers a bivariate mean component", {
  set.seed(129); n <- 70; truth <- 0.6; mu <- c(truth, -0.4); Sigma <- matrix(c(1, .35, .35, 1.4), 2, 2)
  Z <- matrix(rnorm(n * 2), n, 2); Y <- sweep(Z %*% chol(Sigma), 2, mu, "+")
  model <- 'param mu1(1);
block mu1(1) {
double m[2] = {mu1(1), -0.4};
mu1(1) ~ dnorm(0, 3);
for (i = 1:n) Y(i, 1:2) ~ dbvn(m, Sigma);
}'
  d <- hobbs_test_draws(model, list(Y = Y, Sigma = Sigma)); expect_posterior_near(d, "mu1[1]", truth, 0.28)
})

test_that("dbvn gives the correct posterior for a mean component", {
  set.seed(129)
  n <- 70
  truth <- 0.6
  mu <- c(truth, -0.4)
  Sigma <- matrix(c(1, .35, .35, 1.4), 2, 2)
  Z <- matrix(rnorm(n * 2), n, 2)
  Y <- sweep(Z %*% chol(Sigma), 2, mu, "+")

  model <- 'param mu1(1);
block mu1(1) {
double m[2] = {mu1(1), -0.4};
mu1(1) ~ dnorm(0, 3);
for (i = 1:n) Y(i, 1:2) ~ dbvn(m, Sigma);
}'

  d <- hobbs_test_draws(model, list(Y = Y, Sigma = Sigma))

  precision <- solve(Sigma)
  posterior_precision <- 1 / 3^2 + n * precision[1, 1]
  posterior_var <- 1 / posterior_precision
  posterior_mean <- posterior_var * sum(
    precision[1, 1] * Y[, 1] +
      precision[1, 2] * (Y[, 2] - mu[2])
  )
  posterior_sd <- sqrt(posterior_var)

  expect_equal(mean(d$`mu1[1]`), posterior_mean, tolerance = 0.03)
  expect_equal(sd(d$`mu1[1]`), posterior_sd, tolerance = 0.02)
})

