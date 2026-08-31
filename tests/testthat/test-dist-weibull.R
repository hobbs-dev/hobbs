test_that("dweibull recovers its scale", {
  set.seed(115); shape <- 1.8; truth <- 1.4; y <- rweibull(80, shape, truth)
  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dweibull(shape, exp(log_scale(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, shape = shape)); testthat::expect_equal(exp(mean(d[, "log_scale[1]"])), truth, tolerance = 0.28)
})

test_that("dweibull gives the correct posterior for its scale", {
  set.seed(115)
  shape <- 1.8
  truth <- 1.4
  y <- rweibull(80, shape, truth)

  model <- 'param log_scale(1);
block log_scale(1) {
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) y(i) ~ dweibull(shape, exp(log_scale(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, shape = shape))

  log_posterior <- function(log_scale) {
    dnorm(log_scale, 0, 2, log = TRUE) +
      sum(dweibull(y, shape = shape, scale = exp(log_scale), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_scale[1]", log_posterior,
    lower = -1.5, upper = 2,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

