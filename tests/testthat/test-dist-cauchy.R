test_that("dcauchy recovers its location", {
  set.seed(109); truth <- 0.65; y <- rcauchy(90, truth, 1)
  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dcauchy(loc(1), 1.0);
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "loc[1]", truth, 0.38)
})

test_that("dcauchy gives the correct posterior for its location", {
  set.seed(109)
  truth <- 0.65
  y <- rcauchy(90, truth, 1)

  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dcauchy(loc(1), 1.0);
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(loc) {
    dnorm(loc, 0, 3, log = TRUE) +
      sum(dcauchy(y, loc, 1, log = TRUE))
  }

  expect_numerical_posterior(
    d, "loc[1]", log_posterior,
    lower = -2.5, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

