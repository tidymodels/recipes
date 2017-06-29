#' Manually Add Roles
#'
#' \code{add_role} can add a role definition to an existing variable in the
#'   recipe.
#'
#' @param recipe An existing \code{\link{recipe}}.
#' @param ... One or more selector functions to choose which variables are
#'   being assigned a role. See \code{\link{selections}} for more details.
#' @param new_role A character string for a single role.
#' @return An updated recipe object.
#' @details If a variable is selected that currently has a role, the role is
#'   changed and a warning is issued.
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
#' @examples
#'
#' data(biomass)
#'
#' # Create the recipe manually
#' rec <- recipe(x = biomass)
#' rec
#' summary(rec)
#'
#' rec <- rec %>%
#'   add_role(carbon, contains("gen"), sulfur, new_role = "predictor") %>%
#'   add_role(sample, new_role = "id variable") %>%
#'   add_role(dataset, new_role = "splitting variable") %>%
#'   add_role(HHV, new_role = "outcome")
#' rec
#'
#'@importFrom rlang quos
add_role <- function(recipe, ..., new_role = "predictor") {
  if (length(new_role) > 1)
    stop("A single role is required", call. = FALSE)
  terms <- quos(...)
  if (is_empty(terms))
    warning("No selectors were found", call. = FALSE)
  vars <- terms_select(terms = terms, info = summary(recipe))
  ## check if there are newly defined variables in the list
  existing_var <- vars %in% recipe$var_info$variable
  if (any(!existing_var)) {
    ## Add new variable with role
    new_vars <-
      tibble(variable = vars[!existing_var],
             role = rep(new_role, sum(!existing_var)))
    recipe$var_info <- rbind(recipe$var_info, new_vars)
  } else {
    ##   check for current roles that are missing
    vars2 <- vars[existing_var]
    has_role <-
      !is.na(recipe$var_info$role[recipe$var_info$variable %in% vars2])
    if (any(has_role)) {
      warning("Changing role(s) for ",
              paste0(vars2[has_role], collapse = ", "),
              call. = FALSE)
    }
    recipe$var_info$role[recipe$var_info$variable %in% vars2] <-
      new_role
  }
  recipe$term_info <- recipe$var_info
  recipe
}
