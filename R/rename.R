#' Rename variables by name using dplyr
#'
#' `step_rename` creates a *specification* of a recipe step that will add
#'  variables using [dplyr::rename()].
#'
#' @inheritParams step_center
#' @param ... One or more unquoted expressions separated by commas. See
#'  [dplyr::rename()] where the convention is **`new_name = old_name`**.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? By default, the function assumes that the new dimension
#'  columns created by the original variables will be used as predictors in a
#'  model.
#' @param inputs Quosure(s) of `...`.
#' @template step-return
#' @details When an object in the user's global environment is referenced in
#'  the expression defining the new variable(s), it is a good idea to use
#'  quasiquotation (e.g. `!!`) to embed the value of the object in the
#'  expression (to be portable between sessions).
#'
#'  When you [`tidy()`] this step, a tibble with
#'  columns `values` which contains the `rename` expressions as character
#'  strings (and are not reparsable) is returned.
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept transformation_methods
#' @export
#' @examples
#' recipe( ~ ., data = iris) %>%
#'   step_rename(Sepal_Width = Sepal.Width) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   slice(1:5)
#'
#' vars <- c(var1 = "cyl", var2 = "am")
#' car_rec <-
#'   recipe(~ ., data = mtcars) %>%
#'   step_rename(!!vars)
#'
#' car_rec %>%
#'   prep() %>%
#'   bake(new_data = NULL)
#'
#' car_rec %>%
#'   tidy(number = 1)


step_rename <- function(
  recipe, ...,
  role = "predictor",
  trained = FALSE,
  inputs = NULL,
  skip = FALSE,
  id = rand_id("rename")
) {

  inputs <- enquos(..., .named = TRUE)

  add_step(
    recipe,
    step_rename_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_rename_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "rename",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_rename <- function(x, training, info = NULL, ...) {
  step_rename_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_rename <- function(object, new_data, ...) {
  dplyr::rename(new_data, !!!object$inputs)
}


print.step_rename <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Variable renaming for ",
        paste0(names(x$inputs), collapse = ", "))
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_rename` object
#' @export
tidy.step_rename <- function(x, ...) {
  var_expr <- map(x$inputs, quo_get_expr)
  var_expr <- map_chr(var_expr, quo_text, width = options()$width, nlines = 1)
  tibble(
    terms = names(x$inputs),
    value = var_expr,
    id = rep(x$id, length(x$inputs))
  )
}
