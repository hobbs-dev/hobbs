test_that("dt recovers its location", {
  set.seed(110); truth <- -0.6; df <- 5; y <- truth + rt(80, df)
  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dt(df, loc(1), 1.0);
}'
  d <- hobbs_test_draws(model, list(y = y, df = df)); expect_posterior_near(d, "loc[1]", truth, 0.32)
})

test_that("dt gives the correct posterior for its location", {
  set.seed(110)
  truth <- -0.6
  df <- 5
  y <- truth + rt(80, df)

  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dt(df, loc(1), 1.0);
}'

  d <- hobbs_test_draws(model, list(y = y, df = df))

  log_posterior <- function(loc) {
    dnorm(loc, 0, 3, log = TRUE) +
      sum(dt(y - loc, df = df, log = TRUE))
  }

  expect_numerical_posterior(
    d, "loc[1]", log_posterior,
    lower = -3, upper = 2,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

