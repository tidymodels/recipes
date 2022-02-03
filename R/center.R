#' Centering numeric data
#'
#' `step_center` creates a *specification* of a recipe
#'  step that will normalize numeric data to have a mean of zero.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param means A named numeric vector of means. This is
#'  `NULL` until computed by [prep()].
#' @param na_rm A logical value indicating whether `NA`
#'  values should be removed during computations.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @template step-return
#'
#' @family normalization steps
#' @export
#' @details Centering data means that the average of a variable is
#'  subtracted from the data. `step_center` estimates the
#'  variable means from the data used in the `training`
#'  argument of `prep.recipe`. `bake.recipe` then applies
#'  the centering to new data sets using these means.
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#'  `terms` (the selectors or variables selected) and `value` (the means)
#'  is returned.
#'
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' center_trans <- rec %>%
#'   step_center(carbon, contains("gen"), -hydrogen)
#'
#' center_obj <- prep(center_trans, training = biomass_tr)
#'
#' transformed_te <- bake(center_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#'
#' tidy(center_trans, number = 1)
#' tidy(center_obj, number = 1)
step_center <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           means = NULL,
           na_rm = TRUE,
           skip = FALSE,
           id = rand_id("center")) {
    add_step(
      recipe,
      step_center_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        means = means,
        na_rm = na_rm,
        skip = skip,
        id = id
      )
    )
  }

## Initializes a new object
step_center_new <-
  function(terms, role, trained, means, na_rm, skip, id) {
    step(
      subclass = "center",
      terms = terms,
      role = role,
      trained = trained,
      means = means,
      na_rm = na_rm,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_center <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names])

  means <-
    vapply(training[, col_names], mean, c(mean = 0), na.rm = x$na_rm)
  step_center_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    means = means,
    na_rm = x$na_rm,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_center <- function(object, new_data, ...) {
  res <-
    sweep(as.matrix(new_data[, names(object$means)]), 2, object$means, "-")
  res <- tibble::as_tibble(res)
  new_data[, names(object$means)] <- res
  as_tibble(new_data)
}

print.step_center <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Centering for "
    print_step(names(x$means), x$terms, x$trained, title, width)
    invisible(x)
  }


#' @rdname tidy.recipe
#' @export
tidy.step_center <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$means),
                  value = unname(x$means))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}
