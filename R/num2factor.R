#' Convert Numbers to Factors
#'
#' `step_num2factor` will convert one or more numeric
#'  vectors to factors (ordered or unordered). This can be useful
#'  when categories are encoded as integers.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will converted to factors. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.

#' @param role Not used by this step since no new variables are
#'  created.
#' @param transform A function taking a single argument `x` that
#'  can be used to modify the numeric values prior to determining
#'  the levels (perhaps using [base::paste()] or [base::format()]).
#' @param levels A list of values that will be used as the levels.
#'  These are the numeric data converted to character and ordered.
#'  This is `NULL` until computed by [prep.recipe()].
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
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [step_dummy()]
#' @note If `bake` is used on a data set where a new value is
#'  in the column being converted, `bake` will silently give values
#'  of `NA` to these rows (see the example below).
#'
#' @examples
#' iris2 <- iris
#' iris2$Species <- as.numeric(iris2$Species)
#'
#' rec <- recipe(~ ., data = iris2)
#'
#' make_factor <- rec %>% step_num2factor(Species)
#' make_factor <- prep(make_factor,
#'                     training = iris2,
#'                     retain = TRUE)
#'
#' # note that `diet` is a factor
#' juice(make_factor) %>% head
#' okc %>% head
#' tidy(make_factor, number = 1)
#'
#' # When novel values are exposed
#' with_transform <- rec %>%
#'   step_num2factor(Species, transform = function(x) paste0("val_", x))
#'
#' with_transform <- prep(with_transform,
#'                        training = iris2[1:75,])
#' new_values <- bake(with_transform, new_data = iris2, Species)
#' table(new_values[["Species"]], iris2$Species, useNA = "ifany")

step_num2factor <-
  function(recipe,
           ...,
           role = NA,
           transform = function(x) x,
           trained = FALSE,
           levels = NULL,
           ordered = FALSE,
           skip = FALSE,
           id = rand_id("num2factor")) {
    if (!is_tune(ordered) & !is_varying(ordered)) {
      if (!is.logical(ordered) || length(ordered) != 1)
        stop("`ordered` should be a single logical variable")
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

make_factor_num <- function (x, lvl, ord, foo)
  factor(foo(x), levels = lvl, ordered = ord)


#' @export
bake.step_num2factor <- function(object, new_data, ...) {
  col_names <- names(object$ordered)

  new_data[, col_names] <-
    map2_df(new_data[, col_names],
            object$levels,
            make_factor_num,
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

