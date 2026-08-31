test_that("dlaplace recovers its location", {
  set.seed(114); truth <- -0.45; y <- r_laplace(80, truth, 0.7)
  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlaplace(loc(1), 0.7);
}'
  d <- hobbs_test_draws(model, list(y = y)); expect_posterior_near(d, "loc[1]", truth, 0.25)
})

test_that("dlaplace gives the correct posterior for its location", {
  set.seed(114)
  truth <- -0.45
  scale <- 0.7
  y <- r_laplace(80, truth, scale)

  model <- 'param loc(1);
block loc(1) {
loc(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dlaplace(loc(1), 0.7);
}'

  d <- hobbs_test_draws(model, list(y = y))

  log_posterior <- function(loc) {
    dnorm(loc, 0, 3, log = TRUE) -
      sum(abs(y - loc)) / scale
  }

  expect_numerical_posterior(
    d, "loc[1]", log_posterior,
    lower = -2, upper = 2,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

