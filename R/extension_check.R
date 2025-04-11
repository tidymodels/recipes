#' Checks that steps have all S3 methods
#'
#' This is a developer tool intended to help making sure all methods for each
#' step have been created.
#'
#' @param pkg Character, name of package containing steps to check
#' @param exclude_steps Character, name of steps to exclude. This is mostly used
#'   to remove false positives.
#' @param exclude_methods Character, which methods to exclude testing for. Can
#'   take the values "prep", "bake", "print", "tidy", and "required_pkgs".
#'
#' @details
#'
#' It is recommended that the following test in placed in packages that add
#' recipes steps to help keep everything up to date.
#'
#' ```r
#' test_that("recipes_extension_check", {
#'   expect_snapshot(
#'     recipes::recipes_extension_check(
#'       pkg = "pkgname"
#'     )
#'   )
#' })
#' ```
#'
#' @return cli output
#' @export
#'
#' @seealso [developer_functions]
#'
#' @examples
#' recipes_extension_check(
#'   pkg = "recipes"
#' )
#'
#' recipes_extension_check(
#'   pkg = "recipes",
#'   exclude_steps = "step_testthat_helper",
#'   exclude_methods = c("required_pkgs")
#' )
recipes_extension_check <- function(
  pkg,
  exclude_steps = character(),
  exclude_methods = character()
) {
  exclude_methods <- rlang::arg_match(
    exclude_methods,
    values = c("prep", "bake", "print", "tidy", "required_pkgs"),
    multiple = TRUE
  )

  fns <- sort(names(asNamespace(pkg)))

  steps <- grep("^step_", fns, value = TRUE)
  steps <- grep("_new$", steps, value = TRUE, invert = TRUE)

  steps <- setdiff(steps, exclude_steps)

  preps <- !paste0("prep.", steps) %in% fns & !"prep" %in% exclude_methods

  if (any(preps)) {
    cli::cli_alert_info(
      "The following steps doesn't have {.code prep.*} methods:"
    )
    cli::cli_ul(steps[preps])
  }

  bakes <- !paste0("bake.", steps) %in% fns & !"bake" %in% exclude_methods

  if (any(bakes)) {
    cli::cli_alert_info(
      "The following steps doesn't have {.code bake.*} methods:"
    )
    cli::cli_ul(steps[bakes])
  }

  prints <- !paste0("print.", steps) %in% fns & !"print" %in% exclude_methods

  if (any(prints)) {
    cli::cli_alert_info(
      "The following steps doesn't have {.code print.*} methods:"
    )
    cli::cli_ul(steps[prints])
  }

  tidys <- !paste0("tidy.", steps) %in% fns & !"tidy" %in% exclude_methods

  if (any(tidys)) {
    cli::cli_alert_info(
      "The following steps doesn't have {.code tidy.*} methods:"
    )
    cli::cli_ul(steps[tidys])
  }

  required_pkgss <- !paste0("required_pkgs.", steps) %in% fns &
    !"required_pkgs" %in% exclude_methods

  if (any(required_pkgss)) {
    cli::cli_alert_info(
      "The following steps doesn't have {.code required_pkgs.*} methods:"
    )
    cli::cli_ul(steps[required_pkgss])
  }

  if (
    !any(preps) &&
      !any(bakes) &&
      !any(prints) &&
      !any(tidys) &&
      !any(required_pkgss)
  ) {
    cli::cli_alert_success("All steps have all method!")
  }
  cli::cli_end()
}
