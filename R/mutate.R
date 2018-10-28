#' Add new variables using `mutate`
#'
#' `step_mutate` creates a *specification* of a recipe step
#'  that will add variables using [dplyr::mutate()].
#'
#' @inheritParams step_center
#' @param ... Name-value pairs of expressions. See [dplyr::mutate()].
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned? By default, the function assumes
#'  that the new dimension columns created by the original variables
#'  will be used as predictors in a model.
#' @param input Quosure(s) of `...`.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `values` which
#'  contains the `mutate` expressions.
#' @details When an object in the user's global environment is 
#'  referenced in the expression defining the new variable(s), 
#'  it is a good idea to use quasiquotation (e.g. `!!`) to embed
#'  the value of the object in the expression (to be portable
#'  between sessions). See the examples. 
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @examples
#' rec <- 
#'   recipe( ~ ., data = iris) %>%
#'   step_mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#' 
#' prepped <- prep(rec, training = iris %>% slice(1:75), retain = TRUE)
#' 
#' library(dplyr)
#' 
#' dplyr_train <- 
#'   iris %>%
#'   as_tibble() %>%
#'   slice(1:75) %>%
#'   mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#' 
#' rec_train <- juice(prepped)
#' all.equal(dplyr_train, rec_train)
#' 
#' dplyr_test <- 
#'   iris %>%
#'   as_tibble() %>%
#'   slice(76:150) %>%
#'   mutate(
#'     dbl_width = Sepal.Width * 2,
#'     half_length = Sepal.Length / 2
#'   )
#' rec_test <- bake(prepped, iris %>% slice(76:150))
#' all.equal(dplyr_test, rec_test)
#' 
#' # Embedding objects:
#' const <- 1.414
#' 
#' qq_rec <- 
#'   recipe( ~ ., data = iris) %>%
#'   step_mutate(
#'     bad_approach = Sepal.Width * const,
#'     best_approach = Sepal.Width * !!const
#'   ) %>%
#'   prep(training = iris, retain = TRUE)
#' 
#' juice(qq_rec, contains("appro")) %>% slice(1:4)
#' 
#' # The difference:
#' tidy(qq_rec, number = 1)  

step_mutate <- function(
  recipe, ..., 
  role = "predictor", 
  trained = FALSE, 
  input = NULL,
  skip = FALSE,
  id = rand_id("mutate")
) {
  
  inputs <- enquos(...)
  if (is_empty(inputs)) 
    stop("Please supply at least one name-value pair.", call. = FALSE)
  
  add_step(
    recipe, 
    step_mutate_new(
      terms = terms, 
      trained = trained,
      role = role, 
      input = inputs,
      skip = skip,
      id = id
    )
  )
}

step_mutate_new <- 
  function(terms, role, trained, input, skip, id) {
    step(
      subclass = "mutate", 
      terms = terms,
      role = role,
      trained = trained,
      input = input,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_mutate <- function(x, training, info = NULL, ...) {
  step_mutate_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    input = x$input,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom dplyr mutate
#' @export
bake.step_mutate <- function(object, newdata, ...) {
  dplyr::mutate(newdata, !!!object$input)
}


print.step_mutate <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Variable mutation for ")
    printer(names(x$input), names(x$input), x$trained, width = width)
    invisible(x)
  }

#' @importFrom rlang quo_get_expr expr_text
#' @importFrom purrr map map_chr
#' @importFrom dplyr tibble
#' @rdname step_mutate
#' @param x A `step_mutate` object
#' @export
tidy.step_mutate <- function(x, ...) {
  var_expr <- map(x$input, quo_get_expr)
  var_expr <- map_chr(var_expr, expr_text, width = options()$width, nlines = 1)
    tibble(
      terms = names(x$input),
      value = var_expr,
      id = rep(x$id, length(x$input))
    )
}
