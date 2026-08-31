test_that("dbeta recovers its first shape parameter", {
  set.seed(108); truth <- 2.2; b <- 3.0; y <- rbeta(80, truth, b)
  model <- 'param log_a(1);
block log_a(1) {
log_a(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbeta(exp(log_a(1)), b);
}'
  d <- hobbs_test_draws(model, list(y = y, b = b)); testthat::expect_equal(exp(mean(d[, "log_a[1]"])), truth, tolerance = 0.55)
})

test_that("dbeta gives the correct posterior for its first shape parameter", {
  set.seed(108)
  truth <- 2.2
  b <- 3.0
  y <- rbeta(80, truth, b)

  model <- 'param log_a(1);
block log_a(1) {
log_a(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dbeta(exp(log_a(1)), b);
}'

  d <- hobbs_test_draws(model, list(y = y, b = b))

  log_posterior <- function(log_a) {
    dnorm(log_a, 0, 2, log = TRUE) +
      sum(dbeta(y, exp(log_a), b, log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_a[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

