test_that("dunif recovers a centered uniform location", {
  set.seed(104); truth <- 0; y <- runif(80, truth - 1, truth + 1)
  model <- 'param mu(1);
block mu(1) {
mu(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dunif(mu(1) - 1.0, mu(1) + 1.0);
}'
  d <- hobbs_test_draws(model, list(y = y), step = 0.08); expect_posterior_near(d, "mu[1]", truth, 0.12)
})

test_that("dunif gives the correct posterior for a centered location", {
  set.seed(104)
  truth <- 0
  y <- runif(80, truth - 1, truth + 1)

  model <- 'param mu(1);
block mu(1) {
mu(1) ~ dnorm(0, 3);
for (i = 1:n) y(i) ~ dunif(mu(1) - 1.0, mu(1) + 1.0);
}'

  d <- hobbs_test_draws(model, list(y = y), step = 0.08)

  log_posterior <- function(mu) {
    if (any(y < mu - 1) || any(y > mu + 1)) return(-Inf)
    dnorm(mu, 0, 3, log = TRUE)
  }

  expect_numerical_posterior(
    d, "mu[1]", log_posterior,
    lower = -0.5, upper = 0.5,
    mean_tolerance = 0.03, sd_tolerance = 0.02,
    n_grid = 20001L
  )
})

