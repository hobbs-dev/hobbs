test_that("BDF uses the same declarations, parameters, and optional inputs", {
    build <- ode_test_compile(c(
        "param rate(1);",
        "ode decay(1) { dy(1) = -exp(rate(1))*input(1)*y(1); }",
        "block rate(1) {",
        "vec state(1);",
        "vec subject(1);",
        "state(1) = 2;",
        "subject(1) = 0.7;",
        "ode_bdf_tol(decay, state, 0, 1, 1e-10, 1e-12, 100000, subject);",
        "ode_bdf_tol(decay, state, 1, 2, 1e-10, 1e-12, 100000, subject);",
        "target += state(1);", "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_rate(theta, 1); }"
    ))
    on.exit(ode_test_close(build))
    expect_lt(abs(ode_test_call(build, 0) - 2*exp(-1.4)), 2e-7)
    expect_lt(abs(ode_test_call(build, log(2)) - 2*exp(-2.8)), 2e-7)
    expect_lt(abs(ode_test_call(build, 0) - 2*exp(-1.4)), 2e-7)
})

test_that("BDF solves a strongly stiff system within a bounded step budget", {
    model <- c("param beta(1);",
        "ode stiff(1) { dy(1) = -1e6*(y(1)-cos(t))-sin(t); }",
        "block beta(1) {", "vec state(1);", "state(1) = 1;",
        "ode_bdf_tol(stiff, state, 0, 1, 1e-6, 1e-8, 1000);",
        "target += state(1);", "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }")
    build <- ode_test_compile(model)
    expect_equal(ode_test_call(build,0), cos(1), tolerance = 1e-8)
    ode_test_close(build)
    build <- ode_test_compile(gsub("ode_bdf_tol", "ode_rk45_tol", model, fixed = TRUE))
    on.exit(ode_test_close(build))
    expect_identical(ode_test_call(build,0), -Inf)
})

test_that("BDF default controls and equal times are supported", {
    build <- ode_test_compile(c("param beta(1);",
        "ode force(1) { dy(1) = 2*t; }",
        "block beta(1) {", "vec state(1);", "state(1) = 3;",
        "ode_bdf(force, state, 2, 2);",
        "ode_bdf(force, state, 2, 4);",
        "target += state(1);", "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }"))
    on.exit(ode_test_close(build))
    expect_equal(ode_test_call(build,0), 15, tolerance = 1e-6)
})

test_that("BDF handles Robertson kinetics and improves with tighter tolerances", {
    model <- c("param beta(1);",
        "ode reaction(3) {",
        "dy(1) = -0.04*y(1) + 1e4*y(2)*y(3);",
        "dy(3) = 3e7*y(2)*y(2);",
        "dy(2) = -dy(1)-dy(3);", "}",
        "block beta(1) {", "vec state(3);", "state(1) = 1;",
        "ode_bdf_tol(reaction, state, 0, 100, REL_CONTROL, ABS_CONTROL, 100000);",
        "target += state(1);", "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }")
    # Independently computed with Radau at rtol=1e-12, atol=1e-14.
    reference <- 0.617234882396
    loose <- ode_test_compile(gsub("ABS_CONTROL", "1e-6", gsub("REL_CONTROL", "1e-4", model, fixed=TRUE), fixed=TRUE))
    loose_value <- ode_test_call(loose,0)
    ode_test_close(loose)
    tight <- ode_test_compile(gsub("ABS_CONTROL", "1e-12", gsub("REL_CONTROL", "1e-10", model, fixed=TRUE), fixed=TRUE))
    on.exit(ode_test_close(tight))
    tight_value <- ode_test_call(tight,0)
    expect_lt(abs(tight_value-reference), abs(loose_value-reference)/100)
    expect_equal(tight_value, reference, tolerance = 1e-7)
})

test_that("BDF failure rejects a block and does not change native state", {
    model <- c("param beta(1);",
        "ode decay(1) { dy(1) = -y(1); }",
        "block beta(1) {", "vec state(1);", "state(1) = 1;",
        "ode_bdf_tol(decay, state, 0, 10, 1e-6, 1e-8, 1);",
        "target += state(1);", "}",
        "static void native_decay(double t, const double *y, double *dy, void *ctx) { (void)t; (void)ctx; dy[0] = -y[0]; }",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }",
        "hobbs_EXPORT void ode_test_status(double *state, int *status) { *status = hobbs_ode_bdf(native_decay, NULL, 1, state, 0, 10, 1e-6, 1e-8, 1); }")
    build <- ode_test_compile(model)
    on.exit(ode_test_close(build))
    expect_identical(ode_test_call(build,0), -Inf)
    result <- .C(getNativeSymbolInfo("ode_test_status",build$dll), state=2.0, status=integer(1))
    expect_equal(result$state, 2.0)
    expect_equal(result$status, 4L) # HOBBS_ODE_MAX_STEPS
})

test_that("BDF validates syntax and missing derivatives", {
    systems <- hobbs:::extract_ode_declarations("ode a(1) { dy(1) = 0; }")$systems
    expect_error(hobbs:::translate_ode_calls(c("vec s(1);", "ode_bdf(b, s, 0, 1);"), systems, "theta"), "Unknown")
    expect_error(hobbs:::translate_ode_calls(c("vec s(1);", "ode_bdf_tol(a, s, 0, 1);"), systems, "theta"), "Use ode")
    build <- ode_test_compile(c("param beta(1);", "ode missing(2) { dy(1) = 0; }",
        "block beta(1) {", "vec s(2);", "s(1) = 1;",
        "ode_bdf(missing, s, 0, 1);", "target += s(1);", "}",
        "hobbs_EXPORT void ode_test_value(double *theta, double *value) { *value = hobbs_block_beta(theta, 1); }"))
    on.exit(ode_test_close(build))
    expect_identical(ode_test_call(build,0), -Inf)
})
