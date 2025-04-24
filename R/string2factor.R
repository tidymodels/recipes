#' Convert strings to factors
#'
#' @description `step_string2factor()` will convert one or more character
#' vectors to factors (ordered or unordered).
#'
#' _Use this step only in special cases_ (see Details) and instead convert
#' strings to factors before using any tidymodels functions.
#'
#' @inheritParams step_center
#' @param levels An optional specification of the levels to be used for the new
#'   factor. If left `NULL`, the sorted unique values present when `bake` is
#'   called will be used.
#' @param ordered A single logical value; should the factor(s) be ordered?
#' @template step-return
#' @family dummy variable and encoding steps
#' @export
#' @details
#'
#' ## When should you use this step?
#'
#' In most cases, if you are planning to use `step_string2factor()` without
#' setting `levels`, you will be better off converting those character variables
#' to factor variables **before using a recipe**.
#'
#' This can be done using \pkg{dplyr} with the following code
#'
#'  ```r
#'  df <- mutate(df, across(where(is.character), as.factor))
#'  ```
#'
#' During resampling, the complete set of values might not be in the character
#' data. Converting them to factors with `step_string2factor()`  then will
#' misconfigure the levels.
#'
#' If the `levels` argument to `step_string2factor()`is used, it will convert
#' all variables affected by this step to have the same levels. Because of this,
#' you will need to know the full set of level when you define the recipe.
#'
#' Also, note that [prep()] has an option `strings_as_factors` that defaults to
#' `TRUE`. This should be changed so that raw character data will be applied to
#' `step_string2factor()`. However, this step can also take existing factors
#' (but will leave them as-is).
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `ordered` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{ordered}{logical, are factors ordered}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' # convert factor to string to demonstrate
#' Sacramento$city <- as.character(Sacramento$city)
#'
#' rec <- recipe(~ city + zip, data = Sacramento)
#'
#' make_factor <- rec |>
#'   step_string2factor(city)
#'
#' make_factor <- prep(make_factor,
#'   training = Sacramento
#' )
#'
#' make_factor
#'
#' # note that `city` is a factor in recipe output
#' bake(make_factor, new_data = NULL) |> head()
#'
#' # ...but remains a string in the data
#' Sacramento |> head()
step_string2factor <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    levels = NULL,
    ordered = FALSE,
    skip = FALSE,
    id = rand_id("string2factor")
  ) {
    check_character(levels, allow_null = TRUE)

    add_step(
      recipe,
      step_string2factor_new(
        terms = enquos(...),
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

get_ord_lvls <- function(x) {
  sort(unique(x))
}

#' @export
prep.step_string2factor <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))
  check_bool(x$ordered, arg = "ordered")

  if (is.null(x$levels)) {
    res <- lapply(training[, col_names], get_ord_lvls)
  } else {
    res <- x$levels
  }

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
  if (is.factor(x)) {
    return(x)
  }
  factor(x, levels = lvl, ordered = ord)
}

#' @export
bake.step_string2factor <- function(object, new_data, ...) {
  col_names <- names(object$ordered)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    if (is.list(object$levels)) {
      levels <- object$levels[[col_name]]
    } else {
      levels <- object$levels
    }

    new_data[[col_name]] <- make_factor(
      new_data[[col_name]],
      lvl = levels,
      ord = object$ordered[[col_name]]
    )
  }

  new_data
}

print.step_string2factor <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Factor variables from "
    print_step(names(x$ordered), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_string2factor <- function(x, ...) {
  term_names <- sel2char(x$terms)
  p <- length(term_names)
  if (is_trained(x)) {
    res <- tibble(
      terms = term_names,
      ordered = rep(unname(x$ordered), p)
    )
  } else {
    res <- tibble(
      terms = term_names,
      ordered = rep(unname(x$ordered), p)
    )
  }
  res$id <- x$id
  res
}
