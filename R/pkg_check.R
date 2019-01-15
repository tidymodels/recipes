#' Update packages
#'
#' This will check to see if all required packages are installed.
#'
#' @param pkg A character string for the package being checked
#' @param ... Extra arguments to pass to [utils::install.packages()]
#' @return Nothing is returned but a message is printed to the
#'  console about which packages (if any) should be installed along
#'  with code to do so.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' recipes_pkg_check("recipes")
#' }
#' @importFrom rlang quos quo quo_squash
#' @importFrom utils install.packages
recipes_pkg_check <- function(pkg = NULL, ...) {
  good <- rep(TRUE, length(pkg))
  for (i in seq(along = pkg)) {
    tested <- try(find.package(pkg[i]), silent = TRUE)
    if (class(tested)[1] == "try-error")
      good[i] <- FALSE
  }
  if (any(!good)) {
    pkList <- paste(pkg[!good], collapse = ", ")
    msg <- paste0(
      sum(!good),
      ifelse(sum(!good) > 1, " packages are", " package is"),
      " needed for this model and",
      ifelse(sum(!good) > 1, " are", " is"),
      " not installed. (",
      pkList,
      "). ",
      "Start a clean R session then run: "
    )
    cat(msg)
    
    install_opt <- quos(...)
    install_pkg <- pkg[!good]
    if (length(install_pkg) > 1)
      inst_expr <-
      quo(install.packages(c(!!!install_pkg),!!!install_opt))
    else
      inst_expr <-
      quo(install.packages(!!install_pkg,!!!install_opt))
    pkg_str <- deparse(quo_squash(inst_expr))
    cat(pkg_str)
  }
  
  invisible()
}


