test_that("continuous parameter declarations select samplers independently", {
    model <- tempfile(fileext = ".c")
    on.exit(unlink(model), add = TRUE)
    writeLines(c(
        "param beta(p) save=mean sampler=rwmh;",
        "param u(n, p) sampler=slice;",
        "param sigma(1) sampler = slice save = mean;"
    ), model)

    info <- hobbs:::parse_param_declarations(
        model,
        data = list(n = 3L, p = 2L)
    )

    expect_equal(
        vapply(info$spec, `[[`, character(1), "sampler"),
        c("rwmh", "slice", "slice")
    )
    expect_equal(
        vapply(info$spec, `[[`, character(1), "save"),
        c("mean", "chain", "mean")
    )
    expect_equal(vapply(info$spec, `[[`, integer(1), "len"), c(2L, 6L, 1L))
})

test_that("rwmh remains the default continuous sampler", {
    model <- tempfile(fileext = ".c")
    on.exit(unlink(model), add = TRUE)
    writeLines("param beta(4);", model)
    info <- hobbs:::parse_param_declarations(model)
    expect_identical(info$spec[[1L]]$sampler, "rwmh")
})

test_that("dparam rejects continuous sampler modifiers", {
    model <- tempfile(fileext = ".c")
    on.exit(unlink(model), add = TRUE)
    writeLines("dparam z(4, 0, 1) sampler=slice;", model)
    expect_error(
        hobbs:::parse_param_declarations(model),
        "only valid for continuous"
    )
})


test_that("slice code generation emits only the slice hot path", {
    param_info <- list(spec = list(list(
        name = "u", offset = 0L, len = 4L, value_type = "continuous", sampler = "slice"
    )))
    block_info <- list(list(name = "u"))
    generated <- paste(
        hobbs:::generate_scalar_kernels_c(param_info, block_info, list()),
        collapse = "\n"
    )

    expect_match(generated, "hobbs_scalar_probe_u", fixed = TRUE)
    expect_match(generated, "hobbs_scalar_slice_try_u", fixed = TRUE)
    expect_false(grepl("hobbs_sweep_sample_u", generated, fixed = TRUE))
    expect_false(grepl("hobbs_sweep_adapt_u", generated, fixed = TRUE))
})

test_that("slice sampler recovers a normal posterior", {
    set.seed(501)
    truth <- 0.65
    sigma <- 1.2
    prior_sd <- 3
    y <- rnorm(80, truth, sigma)

    model <- '
param mu(1) sampler=slice;

block mu(1) {
  mu(1) ~ dnorm(0, 3);
  for (i = 1:n) y(i) ~ dnorm(mu(1), 1.2);
}
'

    draws <- hobbs_test_draws(
        model,
        list(y = y),
        seed = 5501L,
        samples = 4000L,
        burnin = 1000L
    )

    posterior_var <- 1 / (1 / prior_sd^2 + length(y) / sigma^2)
    posterior_mean <- posterior_var * sum(y) / sigma^2
    posterior_sd <- sqrt(posterior_var)

    expect_equal(mean(draws$`mu[1]`), posterior_mean, tolerance = 0.03)
    expect_equal(sd(draws$`mu[1]`), posterior_sd, tolerance = 0.02)
})

test_that("slice and rwmh can be mixed by parameter declaration", {
    skip_if_hobbs_toolchain_missing()
    wd <- tempfile("hobbs-mixed-samplers-")
    dir.create(wd)
    on.exit(unlink(wd, recursive = TRUE, force = TRUE), add = TRUE)

    model <- '
param a(1) sampler=slice;
param b(1) sampler=rwmh;

block a(1) {
  a(1) ~ dnorm(0.5, 1);
}

block b(1) {
  b(1) ~ dnorm(-0.5, 1);
}
'

    fit <- hobbs(
        model = model,
        samples = 1000L,
        burnin = 500L,
        workdir = wd,
        out = file.path(wd, "chain.bin"),
        seed = 5502L
    )

    expect_identical(unname(fit$parameter_sampler[c("a", "b")]), c("slice", "rwmh"))
    expect_identical(vapply(fit$blocks, `[[`, character(1), "sampler"), c("slice", "rwmh"))

    draws <- read_hobbs(fit)
    expect_equal(mean(draws$`a[1]`), 0.5, tolerance = 0.12)
    expect_equal(mean(draws$`b[1]`), -0.5, tolerance = 0.12)
})


test_that("slice sampler preserves exact transactional caches", {
    set.seed(503)
    truth <- -0.35
    y <- rnorm(60, truth, 1)

    model <- '
param beta(1) sampler=slice;

block beta(1) {
  beta(1) ~ dnorm(0, 3);
  for (i = 1:n) y(i) ~ dnorm(mu(i), 1);
} cache mu(n) {
  for (i = 1:n) mu(i) += beta(1);
} update mu(n) {
  for (i = 1:n) {
    mu(i) += proposal(beta(1)) - current(beta(1));
  }
}
'

    draws <- hobbs_test_draws(
        model,
        list(y = y),
        seed = 5503L,
        samples = 3500L,
        burnin = 1000L
    )

    posterior_var <- 1 / (1 / 9 + length(y))
    posterior_mean <- posterior_var * sum(y)
    expect_equal(mean(draws$`beta[1]`), posterior_mean, tolerance = 0.035)
    expect_equal(sd(draws$`beta[1]`), sqrt(posterior_var), tolerance = 0.02)
})
