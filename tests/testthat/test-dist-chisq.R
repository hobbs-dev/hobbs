test_that("dchisq recovers degrees of freedom", {
  set.seed(111); truth <- 4.5; y <- rchisq(100, truth)
  model <- 'param log_df(1);
block log_df(1) {
log_df(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dchisq(exp(log_df(1)));
}'
  d <- hobbs_test_draws(model, list(y = y)); testthat::expect_equal(exp(mean(d[, "log_df[1]"])), truth, tolerance = 0.70)
})

test_that("dchisq gives the correct posterior for degrees of freedom", {
  set.seed(111)
  truth <- 4.5
  y <- rchisq(100, truth)

  model <- 'param log_df(1);
block log_df(1) {
log_df(1) ~ dnorm(1, 2);
for (i = 1:n) y(i) ~ dchisq(exp(log_df(1)));
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(log_df) {
    dnorm(log_df, 1, 2, log = TRUE) +
      sum(dchisq(y, df = exp(log_df), log = TRUE))
  }

  expect_numerical_posterior(
    d, "log_df[1]", log_posterior,
    lower = -0.5, upper = 3,
    mean_tolerance = 0.04, sd_tolerance = 0.03
  )
})

