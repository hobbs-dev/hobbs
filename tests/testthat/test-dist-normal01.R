test_that("normal01 recovers a location through standardized residuals", {
  set.seed(102); truth <- -0.55; y <- rnorm(120, truth, 1)
  model <- 'param mu(1);
block mu(1) {
mu(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) - mu(1) ~ normal01();
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "mu[1]", truth, 0.25)
})

test_that("normal01 gives the correct normal posterior", {
  set.seed(102)
  truth <- -0.55
  sigma <- 1
  prior_sd <- 3
  n <- 120
  y <- rnorm(n, truth, sigma)

  model <- 'param mu(1);
block mu(1) {
mu(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) - mu(1) ~ normal01();
}'

  d <- hobbs_test_draws(model, list(y = y))

  posterior_var <- 1 / (1 / prior_sd^2 + n / sigma^2)
  posterior_mean <- posterior_var * sum(y) / sigma^2
  posterior_sd <- sqrt(posterior_var)

  expect_equal(mean(d$`mu[1]`), posterior_mean, tolerance = 0.025)
  expect_equal(sd(d$`mu[1]`), posterior_sd, tolerance = 0.015)
})

