#' Convert Numbers to Factors
#'
#' `step_num2factor` will convert one or more numeric vectors to factors
#'  (ordered or unordered). This can be useful when categories are encoded as
#'  integers.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which variables will be
#'  converted to factors. See [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param transform A function taking a single argument `x` that can be used
#'  to modify the numeric values prior to determining the levels (perhaps using
#'  [base::as.integer()]). The output of a function should be an integer that
#'  corresponds to the value of `levels` that should be assigned. If not an
#'  integer, the value will be converted to an integer during `bake()`.
#' @param levels A character vector of values that will be used as the levels.
#'  These are the numeric data converted to character and ordered. This is
#'  modified once [prep.recipe()] is executed.
#' @param ordered A single logical value; should the factor(s) be ordered?
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected) and `ordered`.
#' @keywords datagen
#' @concept preprocessing
#' @concept variable_encodings
#' @concept factors
#' @export
#' @seealso [step_factor2string()], [step_string2factor()], [step_dummy()]
#' @examples
#' library(dplyr)
#' library(modeldata)
#' data(attrition)
#'
#' attrition %>%
#'   group_by(StockOptionLevel) %>%
#'   count()
#'
#' amnt <- c("nothin", "meh", "some", "copious")
#'
#' rec <-
#'   recipe(Attrition ~ StockOptionLevel, data = attrition) %>%
#'   step_num2factor(
#'     StockOptionLevel,
#'     transform = function(x) x + 1,
#'     levels = amnt
#'   )
#'
#' encoded <- rec %>% prep() %>% bake(new_data = NULL)
#'
#' table(encoded$StockOptionLevel, attrition$StockOptionLevel)
#'
#'
#' # an example for binning
#'
#' binner <- function(x) {
#'   x <- cut(x, breaks = 1000 * c(0, 5, 10, 20), include.lowest = TRUE)
#'   # now return the group number
#'   as.numeric(x)
#' }
#'
#' inc <- c("low", "med", "high")
#'
#' rec <-
#'   recipe(Attrition ~ MonthlyIncome, data = attrition) %>%
#'   step_num2factor(
#'     MonthlyIncome,
#'     transform = binner,
#'     levels = inc,
#'     ordered = TRUE
#'   ) %>%
#'   prep()
#'
#' encoded <- bake(rec, new_data = NULL)
#'
#' table(encoded$MonthlyIncome, binner(attrition$MonthlyIncome))
#'
#' # What happens when a value is out of range?
#' ceo <- attrition %>% slice(1) %>% mutate(MonthlyIncome = 10^10)
#'
#' bake(rec, ceo)

step_num2factor <-
  function(recipe,
           ...,
           role = NA,
           transform = function(x) x,
           trained = FALSE,
           levels,
           ordered = FALSE,
           skip = FALSE,
           id = rand_id("num2factor")) {
    if (!is_tune(ordered) & !is_varying(ordered)) {
      if (!is.logical(ordered) || length(ordered) != 1)
        rlang::abort("`ordered` should be a single logical variable")
    }

    if (rlang::is_missing(levels) || !is.character(levels)) {
      rlang::abort("Please provide a character vector of appropriate length for `levels`.")
    }

    add_step(
      recipe,
      step_num2factor_new(
        terms = ellipse_check(...),
        role = role,
        transform = transform,
        trained = trained,
        levels = levels,
        ordered = ordered,
        skip = skip,
        id = id
      )
    )
  }

step_num2factor_new <-
  function(terms, role, transform, trained, levels, ordered, skip, id) {
    step(
      subclass = "num2factor",
      terms = terms,
      role = role,
      transform = transform,
      trained = trained,
      levels = levels,
      ordered = ordered,
      skip = skip,
      id = id
    )
  }

get_ord_lvls_num <- function(x, foo)
  sort(unique(as.character(foo(x))))

#' @export
prep.step_num2factor <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  res <- lapply(training[, col_names], get_ord_lvls_num, foo = x$transform)
  res <- c(res, ..levels = list(x$levels))

  ord <- rep(x$ordered, length(col_names))
  names(ord) <- col_names

  step_num2factor_new(
    terms = x$terms,
    role = x$role,
    transform = x$transform,
    trained = TRUE,
    levels = res,
    ordered = ord,
    skip = x$skip,
    id = x$id
  )
}

make_factor_num <- function(x, lvl, ord, foo) {
  y <- foo(x)
  if (!is.integer(y)) {
    y <- as.integer(y)
  }
  factor(lvl[y], levels = lvl, ordered = ord)
}


#' @export
bake.step_num2factor <- function(object, new_data, ...) {
  col_names <- names(object$ordered)

  lvls <- object$levels[names(object$levels) == "..levels"]
  object$levels <- object$levels[names(object$levels) != "..levels"]

  new_data[, col_names] <-
    map_df(new_data[, col_names],
            make_factor_num,
            lvl = lvls[[1]],
            ord = object$ordered[1],
            foo = object$transform)

  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_num2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Factor variables from ")
    printer(names(x$ordered), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_num2factor
#' @param x A `step_num2factor` object.
#' @export
tidy.step_num2factor <- function(x, ...) {
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

