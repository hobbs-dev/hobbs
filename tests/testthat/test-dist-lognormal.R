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

