#' Update packages
#'
#' This will check to see if all required packages are installed.
#'
#' @param pkg A character string for the package being checked
#' @param ... Extra arguments to pass to [utils::install.packages()]
#' @return Nothing is returned but a message is printed to the console about
#'   which packages (if any) should be installed along with code to do so.
#'
#' @seealso [developer_functions]
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' recipes_pkg_check("recipes")
#' }
recipes_pkg_check <- function(pkg = NULL, ...) {
  good <- rep(TRUE, length(pkg))
  for (i in seq(along.with = pkg)) {
    tested <- try(find.package(pkg[i]), silent = TRUE)
    if (class(tested)[1] == "try-error") {
      good[i] <- FALSE
    }
  }
  if (any(!good)) {
    install_opt <- quos(...)
    install_pkg <- pkg[!good]
    n_install_pkgs <- sum(!good)

    cli::cli_text(
      "{n_install_pkgs} package{?s} ({.pkg {install_pkg}}) {?is/are} needed ",
      "for this step but {?is/are} not installed."
    )

    if (length(install_pkg) > 1) {
      inst_expr <-
        quo(install.packages(c(!!!install_pkg), !!!install_opt))
    } else {
      inst_expr <-
        quo(install.packages(!!install_pkg, !!!install_opt))
    }
    pkg_str <- deparse(quo_squash(inst_expr))
    cli::cli_text("To install run: {.run ", pkg_str, "}")
  }

  invisible()
}
