test_that("dexp recovers its rate", {
  set.seed(105); truth <- 1.7; y <- rexp(70, truth)
  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dexp(exp(log_rate(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "log_rate[1]"])), truth, tolerance = 0.38)
})

test_that("dexp gives the correct posterior for its rate", {
  set.seed(105)
  truth <- 1.7
  y <- rexp(70, truth)

  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dexp(exp(log_rate(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(log_rate) {
    dnorm(log_rate, 0, 2, log = TRUE) +
      sum(dexp(y, rate = exp(log_rate), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_rate[1]", log_posterior,
    lower = -1.5, upper = 2,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

