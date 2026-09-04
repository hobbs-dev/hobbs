test_that("dinvwish recovers an isotropic scale", {
  set.seed(132); n <- 35; k <- 2L; df <- 7; truth <- 1.4; S <- diag(truth, k); W <- matrix_draws_to_rows(r_invwishart(n, df, S))
  model <- 'param log_scale(1);
block log_scale(1) {
double s = exp(log_scale(1));
double S[4] = {s, 0.0, 0.0, s};
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) W(i, 1:4) ~ dinvwish(S, df, 2);
}'
  d <- hobbs_test_draws(model, list(W = W, df = df)); testthat::expect_equal(exp(mean(d[, "log_scale[1]"])), truth, tolerance = 0.38)
})

test_that("dinvwish gives the correct posterior for an isotropic scale", {
  set.seed(132)
  n <- 35
  k <- 2L
  df <- 7
  truth <- 1.4
  S <- diag(truth, k)
  W <- matrix_draws_to_rows(r_invwishart(n, df, S))

  model <- 'param log_scale(1);
block log_scale(1) {
double s = exp(log_scale(1));
double S[4] = {s, 0.0, 0.0, s};
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) W(i, 1:4) ~ dinvwish(S, df, 2);
}'

  d <- hobbs_test_draws(model, list(W = W, df = df))

  trace_inv_sum <- sum(vapply(seq_len(n), function(i) {
    Wi <- matrix(W[i, ], k, k)
    sum(diag(solve(Wi)))
  }, numeric(1)))

  log_posterior <- function(log_scale) {
    s <- exp(log_scale)
    dnorm(log_scale, 0, 2, log = TRUE) +
      0.5 * n * df * k * log_scale -
      0.5 * s * trace_inv_sum
  }

  expect_numerical_posterior(
    d, "log_scale[1]", log_posterior,
    lower = -2, upper = 2.5,
    mean_tolerance = 0.05, sd_tolerance = 0.04
  )
})

test_that("dinvwish matches Stan and JAGS", {
  skip_if_reference_samplers_missing()
  set.seed(132)
  n <- 35
  k <- 2L
  df <- 7
  truth <- 1.4
  S <- diag(truth, k)
  W <- matrix_draws_to_rows(r_invwishart(n, df, S))
  W_array <- rows_to_matrix_array(W, k)
  W_inv_array <- W_array
  for (i in seq_len(n)) W_inv_array[i, , ] <- solve(W_array[i, , ])

  hobbs_model <- 'param log_scale(1);
block log_scale(1) {
double s = exp(log_scale(1));
double S[4] = {s, 0.0, 0.0, s};
log_scale(1) ~ dnorm(0, 2);
for (i = 1:n) W(i, 1:4) ~ dinvwish(S, df, 2);
}'

  stan_model <- '
data {
  int<lower=1> n;
  int<lower=1> k;
  real<lower=0> df;
  array[n] matrix[k, k] W;
}
parameters {
  vector[1] log_scale;
}
transformed parameters {
  matrix[k, k] S;
  S = diag_matrix(rep_vector(exp(log_scale[1]), k));
}
model {
  log_scale[1] ~ normal(0, 2);
  for (i in 1:n) W[i] ~ inv_wishart(df, S);
}'

  jags_model <- '
model {
  log_scale[1] ~ dnorm(0, 0.25)
  s <- exp(log_scale[1])
  for (a in 1:k) {
    for (b in 1:k) {
      R[a, b] <- equals(a, b) * s
    }
  }
  for (i in 1:n)
    W_inv[i, 1:k, 1:k] ~ dwish(R[1:k, 1:k], df)
}'

  d_hobbs <- hobbs_test_draws(hobbs_model, list(W = W, df = df))
  d_stan <- stan_test_draws(
    stan_model,
    list(n = n, k = k, df = df, W = W_array),
    "log_scale"
  )
  d_jags <- jags_test_draws(
    jags_model,
    list(n = n, k = k, df = df, W_inv = W_inv_array),
    "log_scale"
  )

  expect_posterior_matches_reference(d_hobbs, d_stan, d_jags, "log_scale[1]")
})

