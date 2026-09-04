test_that("dmvn recovers a multivariate mean component", {
  set.seed(130); n <- 70; truth <- -0.55; mu <- c(truth, 0.25, -0.2); Sigma <- matrix(c(1,.2,.1,.2,1.2,.25,.1,.25,.9), 3, 3)
  Z <- matrix(rnorm(n * 3), n, 3); Y <- sweep(Z %*% chol(Sigma), 2, mu, "+")
  model <- 'param mu1(1);
block mu1(1) {
double m[3] = {mu1(1), 0.25, -0.2};
mu1(1) ~ dnorm(0, 3);
for (i = 1:n) Y(i, 1:3) ~ dmvn(m, Sigma);
}'
  d <- hobbs_test_draws(model, list(Y = Y, Sigma = Sigma)); expect_posterior_near(d, "mu1[1]", truth, 0.28)
})

test_that("dmvn gives the correct posterior for a mean component", {
  set.seed(130)
  n <- 70
  truth <- -0.55
  mu <- c(truth, 0.25, -0.2)
  Sigma <- matrix(c(1,.2,.1,.2,1.2,.25,.1,.25,.9), 3, 3)
  Z <- matrix(rnorm(n * 3), n, 3)
  Y <- sweep(Z %*% chol(Sigma), 2, mu, "+")

  model <- 'param mu1(1);
block mu1(1) {
double m[3] = {mu1(1), 0.25, -0.2};
mu1(1) ~ dnorm(0, 3);
for (i = 1:n) Y(i, 1:3) ~ dmvn(m, Sigma);
}'

  d <- hobbs_test_draws(model, list(Y = Y, Sigma = Sigma))

  precision <- solve(Sigma)
  posterior_precision <- 1 / 3^2 + n * precision[1, 1]
  posterior_var <- 1 / posterior_precision
  posterior_mean <- posterior_var * sum(
    precision[1, 1] * Y[, 1] +
      precision[1, 2] * (Y[, 2] - mu[2]) +
      precision[1, 3] * (Y[, 3] - mu[3])
  )
  posterior_sd <- sqrt(posterior_var)

  expect_equal(mean(d$`mu1[1]`), posterior_mean, tolerance = 0.03)
  expect_equal(sd(d$`mu1[1]`), posterior_sd, tolerance = 0.02)
})

test_that("dmvn matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(130)
  n <- 70
  truth <- -0.55
  mu <- c(truth, 0.25, -0.2)
  Sigma <- matrix(c(1,.2,.1,.2,1.2,.25,.1,.25,.9), 3, 3)
  Omega <- solve(Sigma)
  Z <- matrix(rnorm(n * 3), n, 3)
  Y <- sweep(Z %*% chol(Sigma), 2, mu, "+")

  hobbs_model <- 'param mu1(1);
block mu1(1) {
double m[3] = {mu1(1), 0.25, -0.2};
mu1(1) ~ dnorm(0, 3);
for (i = 1:n) Y(i, 1:3) ~ dmvn(m, Sigma);
}'

  stan_model <- '
data {
  int<lower=1> n;
  array[n] vector[3] Y;
  matrix[3, 3] Sigma;
}
parameters {
  vector[1] mu1;
}
model {
  vector[3] m;
  m[1] = mu1[1];
  m[2] = 0.25;
  m[3] = -0.2;
  mu1[1] ~ normal(0, 3);
  for (i in 1:n) Y[i] ~ multi_normal(m, Sigma);
}'

  jags_model <- '
model {
  mu1[1] ~ dnorm(0, 0.1111111111111111)
  m[1] <- mu1[1]
  m[2] <- 0.25
  m[3] <- -0.2
  for (i in 1:n) Y[i, 1:3] ~ dmnorm(m[1:3], Omega[1:3, 1:3])
}'

  d_hobbs <- hobbs_test_draws(hobbs_model, list(Y = Y, Sigma = Sigma))
  d_stan <- stan_test_draws(
    stan_model,
    list(n = n, Y = Y, Sigma = Sigma),
    "mu1"
  )
  d_jags <- jags_test_draws(
    jags_model,
    list(n = n, Y = Y, Omega = Omega),
    "mu1"
  )

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "mu1[1]")
})

