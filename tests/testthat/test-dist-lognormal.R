test_that("dlnorm recovers meanlog", {
  set.seed(112); truth <- 0.5; y <- rlnorm(70, truth, 0.6)
  model <- 'param meanlog(1);
block meanlog(1) {
meanlog(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlnorm(meanlog(1), 0.6);
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "meanlog[1]", truth, 0.22)
})

test_that("dlnorm gives the correct posterior for meanlog", {
  set.seed(112)
  truth <- 0.5
  sigma <- 0.6
  prior_sd <- 3
  n <- 70
  y <- rlnorm(n, truth, sigma)

  model <- 'param meanlog(1);
block meanlog(1) {
meanlog(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlnorm(meanlog(1), 0.6);
}'

  d <- hobbs_test_draws(model, list(y = y))

  z <- log(y)
  posterior_var <- 1 / (1 / prior_sd^2 + n / sigma^2)
  posterior_mean <- posterior_var * sum(z) / sigma^2
  posterior_sd <- sqrt(posterior_var)

  expect_equal(mean(d$`meanlog[1]`), posterior_mean, tolerance = 0.025)
  expect_equal(sd(d$`meanlog[1]`), posterior_sd, tolerance = 0.015)
})

test_that("dlnorm matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(112)
  truth <- 0.5
  sigma <- 0.6
  y <- rlnorm(70, truth, sigma)
  n <- length(y)

  hobbs_model <- 'param meanlog(1);
block meanlog(1) {
meanlog(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlnorm(meanlog(1), 0.6);
}'

  stan_model <- '
data {
  int<lower=1> n;
  vector<lower=0>[n] y;
  real<lower=0> sigma;
}
parameters {
  vector[1] meanlog;
}
model {
  meanlog[1] ~ normal(0, 3);
  y ~ lognormal(meanlog[1], sigma);
}'

  jags_model <- '
model {
  meanlog[1] ~ dnorm(0, 0.1111111111111111)
  tau <- 1 / pow(sigma, 2)
  for (i in 1:n) y[i] ~ dlnorm(meanlog[1], tau)
}'

  data <- list(n = n, y = y, sigma = sigma)
  d_hobbs <- hobbs_test_draws(hobbs_model, list(y = y))
  d_stan <- stan_test_draws(stan_model, data, "meanlog")
  d_jags <- jags_test_draws(jags_model, data, "meanlog")

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "meanlog[1]")
})

