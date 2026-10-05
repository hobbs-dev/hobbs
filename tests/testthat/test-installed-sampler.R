# # Isolate installed-executable lookup while exercising real process startup.
# sampler_test_environment <- function(sampler) {
#     env <- new.env(parent = asNamespace("hobbs"))
#     env$hobbs_installed_sampler <- function() sampler
#     env$hobbs_build_sampler <- function(...) stop("Unexpected sampler build")
#     env$hobbs_check_build_tools <- function(...) stop("Unexpected toolchain check")
#     for (name in c("hobbs_check_sampler", "hobbs_require_installed_sampler",
#                    "hobbs_install_sampler")) {
#         fun <- get(name, envir = asNamespace("hobbs"))
#         environment(fun) <- env
#         env[[name]] <- fun
#     }
#     env
# }

# test_that("missing installed samplers do not trigger a build", {
#     env <- sampler_test_environment("")
#     result <- withVisible(env$hobbs_check_sampler(quiet = TRUE))
#     expect_false(result$value)
#     expect_false(result$visible)
#     expect_error(env$hobbs_require_installed_sampler(), "Reinstall the hobbs package")
# })

# test_that("sampler startup and compatibility reuse do not require Rust", {
#     skip_on_os("windows")
#     directory <- tempfile("hobbs sampler with spaces ")
#     dir.create(directory)
#     on.exit(unlink(directory, recursive = TRUE), add = TRUE)
#     sampler <- file.path(directory, "hobbs")
#     writeLines(c("#!/bin/sh", '[ "$1" = "--help" ] || exit 2',
#                  "echo hobbs", "exit 0"), sampler)
#     Sys.chmod(sampler, mode = "0755")
#     env <- sampler_test_environment(sampler)
#     old_path <- Sys.getenv("PATH")
#     on.exit(Sys.setenv(PATH = old_path), add = TRUE)
#     Sys.setenv(PATH = directory)
#     expect_identical(unname(Sys.which("cargo")), "")
#     expect_identical(unname(Sys.which("rustc")), "")
#     result <- withVisible(env$hobbs_check_sampler(quiet = TRUE))
#     expect_true(result$value)
#     expect_false(result$visible)
#     expect_warning(path <- env$hobbs_install_sampler(quiet = TRUE), "deprecated")
#     expect_identical(path, normalizePath(sampler))
# })

# test_that("failed sampler startup returns FALSE and reports the process error", {
#     skip_on_os("windows")
#     sampler <- tempfile("hobbs-failed-")
#     on.exit(unlink(sampler), add = TRUE)
#     writeLines(c("#!/bin/sh", "echo startup-error >&2", "exit 7"), sampler)
#     Sys.chmod(sampler, mode = "0755")
#     env <- sampler_test_environment(sampler)
#     expect_false(env$hobbs_check_sampler(quiet = TRUE))
#     messages <- character()
#     result <- withCallingHandlers(env$hobbs_check_sampler(), message = function(m) {
#         messages <<- c(messages, conditionMessage(m))
#         invokeRestart("muffleMessage")
#     })
#     expect_false(result)
#     expect_match(paste(messages, collapse = "\n"), "status 7")
#     expect_match(paste(messages, collapse = "\n"), "startup-error")
# })
