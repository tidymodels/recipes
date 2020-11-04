#' Convert Strings to Factors
#'
#' `step_string2factor` will convert one or more character
#'  vectors to factors (ordered or unordered).
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be converted to factors. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.

#' @param role Not used by this step since no new variables are
#'  created.
#' @param levels An options specification of the levels to be used
#'  for the new factor. If left `NULL`, the sorted unique
#'  values present when `bake` is called will be used.
#' @param ordered A single logical value; should the factor(s) be
#'  ordered?
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `ordered`.
#' @keywords datagen
#' @concept preprocessing
#' @concept variable_encodings
#' @concept factors
#' @export
#' @details If `levels` is given, `step_string2factor` will
#'  convert all variables affected by this step to have the same
#'  levels.
#'
#'  Also, note that `prep` has an option `strings_as_factors` that
#'  defaults to `TRUE`. This should be changed so that raw character
#'  data will be applied to `step_string2factor`. However, this step
#'  can also take existing factors (but will leave them as-is).
#' @seealso [step_factor2string()] [step_dummy()] [step_other()]
#'  [step_novel()]
#' @examples
#' library(modeldata)
#' data(okc)
#'
#' rec <- recipe(~ diet + location, data = okc)
#'
#' make_factor <- rec %>%
#'   step_string2factor(diet)
#' make_factor <- prep(make_factor,
#'                     training = okc,
#'                     strings_as_factors = FALSE)
#'
#' # note that `diet` is a factor
#' bake(make_factor, new_data = NULL) %>% head
#' okc %>% head
#' tidy(make_factor, number = 1)

step_string2factor <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           levels = NULL,
           ordered = FALSE,
           skip = FALSE,
           id = rand_id("string2factor")) {
    if (!is_tune(ordered) & !is_varying(ordered)) {
      if (!is.logical(ordered) || length(ordered) != 1) {
        rlang::abort("`ordered` should be a single logical variable")
      }
    }
    if ((!is.null(levels) & !is.character(levels)) | is.list(levels)) {
      rlang::abort("`levels` should be NULL or a single character vector")
    }

    add_step(
      recipe,
      step_string2factor_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        levels = levels,
        ordered = ordered,
        skip = skip,
        id = id
      )
    )
  }

step_string2factor_new <-
  function(terms, role, trained, levels, ordered, skip, id) {
    step(
      subclass = "string2factor",
      terms = terms,
      role = role,
      trained = trained,
      levels = levels,
      ordered = ordered,
      skip = skip,
      id = id
    )
  }

get_ord_lvls <- function(x)
  sort(unique(x))

#' @export
prep.step_string2factor <- function(x, training, info = NULL, ...) {
  col_names <- eval_select_recipes(x$terms, training, info)
  str_check <-
    vapply(
      training[, col_names],
      function(x) is.character(x) | is.factor(x),
      logical(1)
    )
  if (any(!str_check))
    rlang::abort(
      paste0(
        "The following variables are not character vectors: ",
        paste0("`", names(str_check)[!str_check], "`", collapse = ", ")
      )
    )

  if (is.null(x$levels)) {
    res <- lapply(training[, col_names], get_ord_lvls)
  } else
    res <- x$levels

  ord <- rep(x$ordered, length(col_names))
  names(ord) <- col_names

  step_string2factor_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    levels = res,
    ordered = ord,
    skip = x$skip,
    id = x$id
  )
}

make_factor <- function(x, lvl, ord) {
  if (is.factor(x))
    return(x)
  factor(x, levels = lvl, ordered = ord)
}

#' @export
bake.step_string2factor <- function(object, new_data, ...) {
  col_names <- names(object$ordered)

  if (is.list(object$levels)) {
    new_data[, col_names] <-
      map2_df(new_data[, col_names],
              object$levels,
              make_factor,
              ord = object$ordered[1])
  } else {
    new_data[, col_names] <-
      map_df(new_data[, col_names],
             make_factor,
             lvl = object$levels,
             ord = object$ordered[1])
  }

  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_string2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Factor variables from ")
    printer(names(x$ordered), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_string2factor
#' @param x A `step_string2factor` object.
#' @export
tidy.step_string2factor <- function(x, ...) {
  term_names <- sel2char(x$terms)
  p <- length(term_names)
  if (is_trained(x)) {
    res <- tibble(terms = term_names,
                  ordered = rep(x$ordered, p))
  } else {
    res <- tibble(terms = term_names,
                  ordered = rep(x$ordered, p))
  }
  res$id <- x$id
  res
}

