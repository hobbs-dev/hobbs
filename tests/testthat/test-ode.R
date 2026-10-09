test_that("ODE declarations and calls validate their syntax", {
    extract <- hobbs:::extract_ode_declarations
    translate <- hobbs:::translate_ode_calls
    expect_error(extract("ode bad(0) { dy(1) = 0; }"), "at least one")
    expect_error(extract(c("ode a(1) { dy(1) = 0; }", "ode a(1) { dy(1) = 1; }")), "Duplicate")
    expect_error(extract("ode bad(1) {"), "Unbalanced")
    systems <- extract("ode a(1) { dy(1) = -y(1); }")$systems
    expect_error(translate(c("vec s(1);", "ode_rk45(b, s, 0, 1);"), systems, "theta"), "Unknown")
    expect_error(translate("ode_rk45(a, s, 0, 1);", systems, "theta"), "local.*vec")
    expect_error(translate(c("vec s(1);", "ode_rk45(a, s, 0);"), systems, "theta"), "Use ode_rk45")
    expect_error(translate(c("vec s(1);", "ode_rk45(a, s, 0, 1, bad);"), systems, "theta"), "input must")
    expect_error(translate(c("vec s(1);", "if (1) ode_rk45(a, s, 0, 1);"), systems, "theta"), "own line")
})

test_that("compiled ODE uses proposed parameters and subject inputs", {
    build <- ode_test_compile(c(
        "param rate(1);",
        "ode decay(1) {",
        "  dy(1) = -exp(rate(1)) * input(1) * y(1);",
        "}",
        "func prediction() {",
        "  vec s(1);",
        "  vec subject(1);",
        "  s(1) = 2;",
        "  subject(1) = 0.7;",
        "  ode_rk45_tol(decay, s, 0, 1, 1e-10, 1e-12, 10000, subject);",
        "  ode_rk45_tol(decay, s, 1, 2, 1e-10, 1e-12, 10000, subject);",
        "  target += s(1);",
        "}",
        "block rate(1) {",
        "  prediction();",
        "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_rate(theta, 1); }"
    ))
    on.exit(ode_test_close(build))
    expect_equal(ode_test_call(build, 0), 2 * exp(-1.4), tolerance = 1e-9)
    expect_equal(ode_test_call(build, log(2)), 2 * exp(-2.8), tolerance = 1e-9)
    expect_equal(ode_test_call(build, 0), 2 * exp(-1.4), tolerance = 1e-9)
})

test_that("multistate ODE supports loops and time-dependent forcing", {
    build <- ode_test_compile(c(
        "param beta(1);",
        "ode system(2) {",
        "  for (i in 1:2) { dy(i) = (i == 1 ? y(2) : -y(1)); }",
        "}",
        "ode force(1) { dy(1) = 2*t; }",
        "block beta(1) {",
        "  vec s(2);",
        "  vec f(1);",
        "  s(1) = 1;",
        "  f(1) = 3;",
        "  ode_rk45_tol(system, s, 0, 10, 1e-10, 1e-12, 100000);",
        "  ode_rk45(force, f, 2, 4);",
        "  target += s(1) + s(2) + f(1);",
        "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }"
    ))
    on.exit(ode_test_close(build))
    expect_equal(ode_test_call(build, 0), cos(10) - sin(10) + 15, tolerance = 1e-8)
})

test_that("failed ODE solves reject the posterior safely", {
    cases <- list(
        list(dim = 1, body = "dy(1) = -y(1);", args = "0, 10, 1e-12, 1e-14, 1", size = 1),
        list(dim = 1, body = "dy(1) = -y(1);", args = "1, 0, 1e-6, 1e-8, 100", size = 1),
        list(dim = 1, body = "dy(1) = INFINITY;", args = "0, 1, 1e-6, 1e-8, 100", size = 1),
        list(dim = 2, body = "dy(1) = 0;", args = "0, 1, 1e-6, 1e-8, 100", size = 2),
        list(dim = 2, body = "dy(1) = 0; dy(2) = 0;", args = "0, 1, 1e-6, 1e-8, 100", size = 1),
        list(dim = 1, body = "dy(1) = input(1);", args = "0, 1, 1e-6, 1e-8, 100", size = 1),
        list(dim = 1, body = "dy(1) = 0;", args = "0, 1, 1e-6, 1e-8, 1.5", size = 1)
    )
    for (case in cases) {
        build <- ode_test_compile(c("param beta(1);",
            sprintf("ode system(%s) { %s }", case$dim, case$body),
            "block beta(1) {", sprintf("vec s(%s);", case$size), "s(1) = 1;",
            sprintf("ode_rk45_tol(system, s, %s);", case$args), "target += s(1);", "}",
            "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }"))
        result <- ode_test_call(build, 0)
        ode_test_close(build)
        expect_identical(result, -Inf)
    }
})

test_that("scalar data dimensions and equal times work", {
    build <- ode_test_compile(c("param beta(1);",
        "ode system(n_state) { dy(1) = times(1)*y(2); dy(2) = -times(2)*y(1); }",
        "block beta(1) {", "vec s(2);", "s(1) = 1;",
        "ode_rk45(system, s, 0, 0);", "ode_rk45_tol(system, s, 0, 2, 1e-10, 1e-12, 10000);", "target += s(1);", "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }"), data = list(n_state = 2L, times = c(1, 1)))
    on.exit(ode_test_close(build))
    expect_equal(ode_test_call(build, 0), cos(2), tolerance = 1e-9)
})
