#' Ratio Variable Creation
#'
#' `step_ratio` creates a *specification* of a recipe
#'  step that will create one or more ratios out of numeric
#'  variables.
#'
#' @inheritParams step_date
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables will be used in the *numerator* of the ratio.
#'  When used with `denom_vars`, the dots indicate which
#'  variables are used in the *denominator*. See
#'  [selections()] for more details.
#' @param denom A call to `denom_vars` to specify which
#'  variables are used in the denominator that can include specific
#'  variable names separated by commas or different selectors (see
#'  [selections()]). If a column is included in both lists
#'  to be numerator and denominator, it will be removed from the
#'  listing.
#' @param naming A function that defines the naming convention for
#'  new ratio columns.
#' @param columns The column names used in the ratios. This
#'  argument is not populated until [prep()] is
#'  executed.
#' @template step-return
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the selectors or variables selected) and `denom` is returned.
#'
#' @template case-weights-not-supported
#'
#' @family multivariate transformation steps
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' data(biomass, package = "modeldata")
#'
#' biomass$total <- apply(biomass[, 3:7], 1, sum)
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
#'   sulfur + total,
#' data = biomass_tr
#' )
#'
#' ratio_recipe <- rec %>%
#'   # all predictors over total
#'   step_ratio(all_numeric_predictors(), denom = denom_vars(total)) %>%
#'   # get rid of the original predictors
#'   step_rm(all_predictors(), -ends_with("total"))
#'
#' ratio_recipe <- prep(ratio_recipe, training = biomass_tr)
#'
#' ratio_data <- bake(ratio_recipe, biomass_te)
#' ratio_data
step_ratio <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           denom = denom_vars(),
           naming = function(numer, denom) {
             make.names(paste(numer, denom, sep = "_o_"))
           },
           columns = NULL,
           keep_original_cols = TRUE,
           skip = FALSE,
           id = rand_id("ratio")) {
    if (is_empty(denom)) {
      rlang::abort(
        paste0(
          "Please supply at least one denominator variable specification. ",
          "See ?selections."
        )
      )
    }
    add_step(
      recipe,
      step_ratio_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        denom = denom,
        naming = naming,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_ratio_new <-
  function(terms, role, trained, denom, naming, columns,
           keep_original_cols, skip, id) {
    step(
      subclass = "ratio",
      terms = terms,
      role = role,
      trained = trained,
      denom = denom,
      naming = naming,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_ratio <- function(x, training, info = NULL, ...) {
  col_names <- expand.grid(
    top = recipes_eval_select(x$terms, training, info),
    bottom = recipes_eval_select(x$denom, training, info),
    stringsAsFactors = FALSE
  )
  col_names <- tibble::as_tibble(col_names)
  col_names <- col_names[!(col_names$top == col_names$bottom), ]

  check_type(
    training[, c(col_names$top, col_names$bottom)],
    types = c("double", "integer")
  )

  step_ratio_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    denom = x$denom,
    naming = x$naming,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ratio <- function(object, new_data, ...) {
  check_new_data(unname(object$columns$top), object, new_data)

  res <- purrr::map2(
    new_data[, object$columns$top],
    new_data[, object$columns$bottom],
    `/`
  )

  names(res) <- apply(
    object$columns,
    MARGIN = 1,
    function(x) object$naming(x[1], x[2])
  )

  res <- tibble::new_tibble(res, nrow = nrow(new_data))

  res <- check_name(res, new_data, object, names(res))
  new_data <- bind_cols(new_data, res)

  keep_original_cols <- get_keep_original_cols(object)
  if (!keep_original_cols) {
    union_cols <- union(object$columns$top, object$columns$bottom)
    new_data <- new_data[, !(colnames(new_data) %in% union_cols), drop = FALSE]
  }
  new_data
}

print.step_ratio <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Ratios from "
    vars <- c(unique(x$columns$top), unique(x$columns$bottom))
    print_step(vars, c(x$terms, x$denom), x$trained, title, width)
    invisible(x)
  }

#' @export
#' @rdname step_ratio
denom_vars <- function(...) quos(...)

#' @rdname tidy.recipe
#' @export
tidy.step_ratio <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = unname(x$columns$top),
      denom = unname(x$columns$bottom)
    )
  } else {
    res <- tidyr::crossing(
      terms = sel2char(x$terms),
      denom = sel2char(x$denom)
    )
    res <- as_tibble(res)
  }
  res$id <- x$id
  arrange(res, terms, denom)
}
