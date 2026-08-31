test_that("dgamma recovers its rate", {
  set.seed(106); shape <- 2.5; truth <- 1.4; y <- rgamma(70, shape, rate = truth)
  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dgamma(shape, exp(log_rate(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, shape = shape)); testthat::expect_equal(exp(mean(d[, "log_rate[1]"])), truth, tolerance = 0.30)
})

test_that("dgamma gives the correct posterior for its rate", {
  set.seed(106)
  shape <- 2.5
  truth <- 1.4
  y <- rgamma(70, shape, rate = truth)

  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dgamma(shape, exp(log_rate(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, shape = shape))

  log_posterior <- function(log_rate) {
    dnorm(log_rate, 0, 2, log = TRUE) +
      sum(dgamma(y, shape = shape, rate = exp(log_rate), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_rate[1]", log_posterior,
    lower = -1, upper = 2,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

