#' Ratio variable creation
#'
#' `step_ratio()` creates a *specification* of a recipe step that will create
#' one or more ratios from selected numeric variables.
#'
#' @inheritParams step_date
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which variables will be
#'   used in the *numerator* of the ratio. When used with `denom_vars`, the dots
#'   indicate which variables are used in the *denominator*. See [selections()]
#'   for more details.
#' @param denom Bare names that specifies which variables are used in the
#'   denominator that can include specific variable names separated by commas or
#'   different selectors (see [selections()]). Can also be a strings or
#'   tidyselect for backwards compatibility If a column is included in both
#'   lists to be numerator and denominator, it will be removed from the listing.
#' @param naming A function that defines the naming convention for new ratio
#'   columns.
#' @template step-return
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `denom` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{denom}{character, name of denominator selected}
#'   \item{id}{character, id of this step}
#' }
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
#' ratio_recipe <- rec |>
#'   # all predictors over total
#'   step_ratio(all_numeric_predictors(), denom = total,
#'              keep_original_cols = FALSE)
#'
#' ratio_recipe <- prep(ratio_recipe, training = biomass_tr)
#'
#' ratio_data <- bake(ratio_recipe, biomass_te)
#' ratio_data
step_ratio <-
  function(
    recipe,
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
    id = rand_id("ratio")
  ) {
    add_step(
      recipe,
      step_ratio_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        denom = enquos(denom),
        naming = naming,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_ratio_new <-
  function(
    terms,
    role,
    trained,
    denom,
    naming,
    columns,
    keep_original_cols,
    skip,
    id
  ) {
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
    bottom = recipes_argument_select(
      x$denom,
      training,
      info,
      single = FALSE,
      arg_name = "denom"
    ),
    stringsAsFactors = FALSE
  )
  col_names <- tibble::as_tibble(col_names)
  col_names <- col_names[!(col_names$top == col_names$bottom), ]

  check_type(
    training[, unique(c(col_names$top, col_names$bottom))],
    types = c("double", "integer")
  )
  check_function(x$naming, arg = "naming")

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
  col_names <- purrr::pmap(object$columns, c)
  unique_col_names <- unique(unlist(col_names))
  check_new_data(unique_col_names, object, new_data)

  res <- list()

  for (col_name in col_names) {
    value <- new_data[[col_name[["top"]]]] / new_data[[col_name[["bottom"]]]]
    res <- c(res, list(value))
  }

  names(res) <- vapply(
    col_names,
    FUN.VALUE = character(1),
    function(x) object$naming(x[1], x[2])
  )

  res <- tibble::new_tibble(res, nrow = nrow(new_data))

  res <- check_name(res, new_data, object, names(res))
  new_data <- vec_cbind(new_data, res, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, unique_col_names)

  new_data
}

#' @export
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
