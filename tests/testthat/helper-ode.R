ode_test_compile <- function(model, data = NULL) {
    skip_if(!nzchar(Sys.which("cc")) && !nzchar(Sys.which("gcc")) && !nzchar(Sys.which("clang")), "C compiler unavailable")
    wd <- tempfile("hobbs-ode-test-")
    dir.create(wd)
    source <- file.path(wd, "model.c")
    writeLines(model, source)
    info <- hobbs:::parse_param_declarations(source, data)
    source <- hobbs:::inline_func_declarations(source, wd)
    blocks <- hobbs:::parse_block_declarations(source)
    data_info <- hobbs:::materialize_data(hobbs:::add_implicit_data_dimensions(data), wd)
    wrapped <- hobbs:::prepare_model_translation_unit(source, wd,
        data_spec = data_info$spec, param_info = info, block_info = blocks,
        allow_block_only = TRUE)
    if (!is.null(data)) write(c(
        "hobbs_EXPORT void ode_test_init(char **path, int *status) { *status = posterior_init(*path); }",
        "hobbs_EXPORT void ode_test_free(void) { posterior_free(); }"), file = wrapped, append = TRUE)
    lib <- hobbs:::compile_c_model(wrapped, wd, quiet = TRUE)
    dll <- dyn.load(lib)
    if (!is.null(data)) {
        status <- .C(getNativeSymbolInfo("ode_test_init", dll),
                     path = data_info$path, status = integer(1))$status
        stopifnot(status == 0L)
    }
    list(wd = wd, dll = dll, has_data = !is.null(data))
}

ode_test_close <- function(build) {
    if (build$has_data) .C(getNativeSymbolInfo("ode_test_free", build$dll))
    dyn.unload(build$dll[["path"]])
    unlink(build$wd, recursive = TRUE)
}

ode_test_call <- function(build, theta) {
    .C(getNativeSymbolInfo("ode_test_value", build$dll),
       theta = as.double(theta), value = double(1))$value
}

