test_that("dpareto recovers alpha", {
  set.seed(116); xmin <- 1; truth <- 3.2; y <- r_pareto(100, xmin, truth)
  model <- 'param log_alpha(1);
block log_alpha(1) {
log_alpha(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dpareto(xmin, exp(log_alpha(1)));
}'
  d <- hobbs_test_draws(model, list(y = y, xmin = xmin)); testthat::expect_equal(exp(mean(d[, "log_alpha[1]"])), truth, tolerance = 0.70)
})

test_that("dpareto gives the correct posterior for alpha", {
  set.seed(116)
  xmin <- 1
  truth <- 3.2
  y <- r_pareto(100, xmin, truth)

  model <- 'param log_alpha(1);
block log_alpha(1) {
log_alpha(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dpareto(xmin, exp(log_alpha(1)));
}'

  d <- hobbs_test_draws(model, list(y = y, xmin = xmin))

  log_posterior <- function(log_alpha) {
    alpha <- exp(log_alpha)
    dnorm(log_alpha, 1, 2, log = TRUE) +
      length(y) * log(alpha) +
      length(y) * alpha * log(xmin) -
      (alpha + 1) * sum(log(y))
  }

  expect_numerical_posterior(
    d, "log_alpha[1]", log_posterior,
    lower = -0.5, upper = 2.5,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

