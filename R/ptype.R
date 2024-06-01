#' Prototype of recipe object
#'
#' This helper function returns the prototype of the input data set expected by
#' the recipe object.
#'
#' @param x A `recipe` object.
#' @param ... currently not used.
#' @param stage A single character. Must be one of `"prep"` or `"bake"`. See
#'   details for more. Defaults to `"prep"`.
#'
#' @details
#' The returned ptype is a tibble of the data set that the recipe object is 
#' expecting. The specifics of which columns depend on the `stage`. 
#' 
#' At `prep()` time, when `stage = "prep"`, the ptype is the data passed to 
#' `recipe()`. The following code chunk represents a possible recipe scenario. 
#' Calling `recipes_ptype(rec_spec, stage = "prep")` and 
#' `recipes_ptype(rec_prep, stage = "prep")` both returns a ptype tibble
#' correspodning to `data_ptype`. This information is used internally in 
#' `prep()` to verify that `data_training` has the right columns with the right
#' types.
#' 
#' ```r
#' rec_spec <- recipe(outcome ~ ., data = data_ptype) %>%
#'   step_normalize(all_numeric_predictors()) %>%
#'   step_dummy(all_nominal_predictors()) 
#' 
#' rec_prep <- prep(rec_spec, training = data_training)
#' ```
#' 
#' At `bake()` time, when `stage = "bake"`, the ptype representents the data
#' that are required for `bake()` to run.  
#' 
#' ```r
#' data_bake <- bake(rec_prep, new_data = data_testing)
#' ```
#' 
#' What this means in practice is that unless otherwise specified, everything
#' but outcomes and case weights are required. These requirements can be changed
#' with `update_role_requirements()` and `recipes_ptype()` respects those 
#' changes.
#' 
#' Note that the order of the columns aren't guaranteed to align with
#' `data_ptype` as the data internally is ordered according to roles.
#' 
#' @return A zero row tibble.
#' @keywords internal
#'
#' @seealso [developer_functions]
#' 
#' @examples
#' training <- tibble(
#'   y = 1:10,
#'   id = 1:10,
#'   x1 = letters[1:10],
#'   x2 = factor(letters[1:10]),
#'   cw = hardhat::importance_weights(1:10)
#' )
#' training
#' 
#' rec_spec <- recipe(y ~ ., data = training)
#' 
#' # outcomes and case_weights are not requred at bake time
#' recipes_ptype(rec_spec, stage = "prep")
#' recipes_ptype(rec_spec, stage = "bake")
#' 
#' rec_spec <- recipe(y ~ ., data = training) %>%
#'   update_role(x1, new_role = "id")
#' 
#' # outcomes and case_weights are not requred at bake time
#' # "id" column is assumed to be needed
#' recipes_ptype(rec_spec, stage = "prep")
#' recipes_ptype(rec_spec, stage = "bake")
#' 
#' rec_spec <- recipe(y ~ ., data = training) %>%
#'   update_role(x1, new_role = "id") %>%
#'   update_role_requirements("id", bake = FALSE)
#' 
#' # update_role_requirements() is used to specify that "id" isn't needed
#' recipes_ptype(rec_spec, stage = "prep")
#' recipes_ptype(rec_spec, stage = "bake")
#' 
#' @export
recipes_ptype <- function(x, ..., stage = "prep") {
  check_dots_empty0(...)

  if (is.null(x$ptype)) {
   cli::cli_abort(
      c(
        x = "Doesn't work on recipes created prior to version 1.1.0.",
        i = "Please recreate recipe."
      )
    )
  }

  ptype <- x$ptype

  stage <- rlang::arg_match(stage, values = c("prep", "bake"))

  if (stage == "bake") {
    required_roles <- compute_bake_role_requirements(x)

    var_info <- x$var_info
    roles <- var_info$role
    roles <- chr_explicit_na(roles)
    
    required_var <- var_info$variable[required_roles[roles]]

    ptype <- ptype[names(ptype) %in% required_var]
  }
  
  ptype
}