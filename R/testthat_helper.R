# This step is created solely for testing purposes
step_testthat_helper <-
  function(
    recipe,
    ...,
    output = NULL,
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = rand_id("testthat_helper")
  ) {
    add_step(
      recipe,
      step_testthat_helper_new(
        terms = enquos(...),
        output = output,
        role = role,
        trained = trained,
        skip = skip,
        id = id
      )
    )
  }

step_testthat_helper_new <-
  function(terms, output, role, trained, skip, id) {
    step(
      subclass = "testthat_helper",
      terms = terms,
      output = output,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_testthat_helper <- function(x, training, info = NULL, ...) {
  step_testthat_helper_new(
    terms = x$terms,
    output = x$output,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_testthat_helper <- function(object, new_data, ...) {
  object$output %||% new_data
}
