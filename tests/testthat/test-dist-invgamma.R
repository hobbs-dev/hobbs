test_that("dinvgamma recovers its rate", {
  set.seed(107); shape <- 4; truth <- 2.2; y <- 1 / rgamma(70, shape, rate = truth)
  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dinvgamma(shape, exp(log_rate(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, shape = shape)); testthat::expect_equal(exp(mean(d[, "log_rate[1]"])), truth, tolerance = 0.40)
})

test_that("dinvgamma gives the correct posterior for its rate", {
  set.seed(107)
  shape <- 4
  truth <- 2.2
  y <- 1 / rgamma(70, shape, rate = truth)

  model <- 'param log_rate(1);
block log_rate(1) {
log_rate(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dinvgamma(shape, exp(log_rate(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, shape = shape))

  log_posterior <- function(log_rate) {
    rate <- exp(log_rate)
    dnorm(log_rate, 0, 2, log = TRUE) +
      length(y) * shape * log_rate -
      rate * sum(1 / y)
  }

  expect_numerical_posterior(
    d, "log_rate[1]", log_posterior,
    lower = -1, upper = 2.5,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

