#' Ratio Variable Creation
#'
#' `step_ratio` creates a a *specification* of a recipe
#'  step that will create one or more ratios out of numeric
#'  variables.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used in the *numerator* of the ratio.
#'  When used with `denom_vars`, the dots indicates which
#'  variables are used in the *denominator*. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For terms created by this step, what analysis role
#'  should they be assigned?. By default, the function assumes that
#'  the newly created ratios created by the original variables will
#'  be used as predictors in a model.
#' @param denom A call to `denom_vars` to specify which
#'  variables are used in the denominator that can include specific
#'  variable names separated by commas or different selectors (see
#'  [selections()]). If a column is included in both lists
#'  to be numerator and denominator, it will be removed from the
#'  listing.
#' @param naming A function that defines the naming convention for
#'  new ratio columns.
#' @param columns The column names used in the ratios. This
#'  argument is not populated until [prep.recipe()] is
#'  executed.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `denom`.
#' @keywords datagen
#' @concept preprocessing
#' @export
#' @examples
#' library(recipes)
#' data(biomass)
#'
#' biomass$total <- apply(biomass[, 3:7], 1, sum)
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
#'                     sulfur + total,
#'               data = biomass_tr)
#'
#' ratio_recipe <- rec %>%
#'   # all predictors over total
#'   step_ratio(all_predictors(), denom = denom_vars(total)) %>%
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
           naming = function(numer, denom)
             make.names(paste(numer, denom, sep = "_o_")),
           columns = NULL,
           skip = FALSE,
           id = rand_id("ratio")) {
    if (is_empty(denom))
      stop("Please supply at least one denominator variable specification. ",
           "See ?selections.", call. = FALSE)
    add_step(
      recipe,
      step_ratio_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        denom = denom,
        naming = naming,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_ratio_new <-
  function(terms, role, trained, denom, naming, columns, skip, id) {
    step(
      subclass = "ratio",
      terms = terms,
      role = role,
      trained = trained,
      denom = denom,
      naming = naming,
      columns = columns,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_ratio <- function(x, training, info = NULL, ...) {
  col_names <- expand.grid(
    top = terms_select(x$terms, info = info),
    bottom = terms_select(x$denom, info = info),
    stringsAsFactors = FALSE
  )
  col_names <- col_names[!(col_names$top == col_names$bottom), ]

  if (nrow(col_names) == 0)
    stop("No variables were selected for making ratios", call. = FALSE)
  if (any(info$type[info$variable %in% col_names$top] != "numeric"))
    stop("The ratio variables should be numeric")
  if (any(info$type[info$variable %in% col_names$bottom] != "numeric"))
    stop("The ratio variables should be numeric")

  step_ratio_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    denom = x$denom,
    naming = x$naming,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ratio <- function(object, new_data, ...) {
  res <- new_data[, object$columns$top] /
    new_data[, object$columns$bottom]
  colnames(res) <-
    apply(object$columns, 1, function(x)
      object$naming(x[1], x[2]))
  if (!is_tibble(res))
    res <- as_tibble(res)

  new_data <- bind_cols(new_data, res)
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_ratio <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Ratios from ")
    if (x$trained) {
      vars <- c(unique(x$columns$top), unique(x$columns$bottom))
      cat(format_ch_vec(vars, width = width))
    } else
      cat(format_selectors(c(x$terms, x$denom), width = width))
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @export
#' @rdname step_ratio
denom_vars <- function(...) quos(...)

#' @rdname step_ratio
#' @param x A `step_ratio` object
#' @export
tidy.step_ratio <- function(x, ...) {
  if (is_trained(x)) {
    res <- x$columns
    colnames(res) <- c("terms", "denom")
    res <- as_tibble(res)
  } else {
    res <- expand.grid(terms = sel2char(x$terms),
                       denom = sel2char(x$denom),
                       stringsAsFactors = FALSE)
    res <- as_tibble(res)
  }
  res$id <- x$id
  res
}

