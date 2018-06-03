#' Convert Strings to Factors
#'
#' `step_string2factor` will convert one or more character
#'  vectors to factors (ordered or unordered).
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will converted to factors. See
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
#' @concept preprocessing variable_encodings factors
#' @export
#' @details If `levels` is given, `step_string2factor` will
#'  convert all factors to have the same levels. Also, note that
#'  `prep` has an option `stringsAsFactors` that defaults
#'  to `TRUE`. This should be changed so that raw character
#'  data will be applied to `step_string2factor`.
#' @seealso [step_factor2string()] [step_dummy()] [step_other()]
#'  [step_novel()]
#' @examples
#' data(okc)
#'
#' rec <- recipe(~ diet + location, data = okc)
#'
#' make_factor <- rec %>%
#'   step_string2factor(diet)
#' make_factor <- prep(make_factor,
#'                     training = okc,
#'                     stringsAsFactors = FALSE,
#'                     retain = TRUE)
#'
#' # note that `diet` is a factor
#' juice(make_factor) %>% head
#' okc %>% head
#' tidy(make_factor, number = 1)

step_string2factor <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           levels = NULL,
           ordered = FALSE,
           skip = FALSE) {
    if(!is.logical(ordered) || length(ordered) != 1)
      stop("`ordered` should be a single logical variable")
    if((!is.null(levels) & !is.character(levels)) | is.list(levels))
      stop("`levels` should be NULL or a single character vector")

    add_step(
      recipe,
      step_string2factor_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        levels = levels,
        ordered = ordered,
        skip = skip
      )
    )
  }

step_string2factor_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           levels = NULL,
           ordered = NULL,
           skip = FALSE
  ) {
    step(
      subclass = "string2factor",
      terms = terms,
      role = role,
      trained = trained,
      levels = levels,
      ordered = ordered,
      skip = skip
    )
  }

get_ord_lvls <- function(x)
  sort(unique(x))

#' @export
prep.step_string2factor <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  str_check <-
    vapply(training[, col_names], is.character, logical(1))
  if (any(!str_check))
    stop(
      "The following variables are not character vectors: ",
      paste0("`", names(str_check)[!str_check], "`", collapse = ", "),
      call. = FALSE
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
    skip = x$skip
  )
}

make_factor <- function(x, lvl, ord) {
  factor(x, levels = lvl, ordered = ord)
}

#' @importFrom purrr map2_df map_df
#' @export
bake.step_string2factor <- function(object, newdata, ...) {
  col_names <- names(object$ordered)

  if (is.list(object$levels)) {
    newdata[, col_names] <-
      map2_df(newdata[, col_names],
              object$levels,
              make_factor,
              ord = object$ordered[1])
  } else {
    newdata[, col_names] <-
      map_df(newdata[, col_names],
             make_factor,
             lvl = object$levels,
             ord = object$ordered[1])
  }

  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}

print.step_string2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Factor variables from ")
    printer(names(x$ordered), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_string2factor
#' @param x A `step_string2factor` object.
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
  res
}

