library(glue)

# set up the boilerplate for a new step or check
# creates a prefilled script in /R
# and an empty script in /tests
# consider using @inheritParams where appropriate instead of full boilerplate
make_new <- function(name,
                     which = c("step", "check")) {
  which <- match.arg(which)
  stopifnot(is.character(name))

  in_recipes_root <-
    tail(stringr::str_split(getwd(), "/")[[1]], 1) == "recipes"
  if (!in_recipes_root) {
    rlang::abort("Change working directory to package root")
  }

  if (glue::glue("{name}.R") %in% list.files("./R")) {
    rlang::abort("step or check already present with this name in /R")
  }

  boilerplate <-
    glue("
{create_documentation(name, which)}
{create_function(name, which)}
{create_generator(name, which)}
{create_prep_method(name, which)}
{create_bake_method(name, which)}
{create_print_method(name, which)}
{create_tidy_method(name, which)}
    ")

  file.create(glue("./R/{name}.R"))
  cat(boilerplate, file = glue("./R/{name}.R"))
  file.create(glue("./tests/testthat/test_{name}.R"))
}

create_documentation <- function(name,
                                 which) {
  glue("
#' <Title>
#'
#' `{which}_{name}` creates a *specification* of a recipe
#'  {which} that <what it does>
#'
#' @param recipe A recipe object. The {which} will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created. <change if role is used>
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' <additional args here>
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @return <describe return>
#'
#' @export
#' @details <describe details>
#'
#' @examples

  ")
}

create_function <- function(name, which) {
  glue('
{which}_{name} <-
    function(recipe,
             ...,
             role = NA,
             trained = FALSE,
             <additional args here>
             skip = FALSE,
             id = rand_id("{name}")) {{
      add_{which}(
        recipe,
        {which}_{name}_new(
          terms = ellipse_check(...),
          trained = trained,
          role = role,
          <additional args here>
          skip = skip,
          id = id
        )
      )
    }}

')
}

create_generator <- function(name, which) {
  glue('
  {which}_{name}_new <-
    function(terms, role, <additional args here>, na_rm, skip, id) {{
      step(
        subclass = "{name}",
        terms = terms,
        role = role,
        trained = trained,
        <additional args here>
        skip = skip,
        id = id
      )
    }}

  ')
}

create_prep_method <- function(name, which) {
  glue('
prep.{which}_{name} <- function(x, training, info = NULL, ...) {{
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names])

  <prepping action here>

  {which}_{name}_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    <additional args here>
    skip = x$skip,
    id = x$id
  )
}}

')
}

create_bake_method <- function(name, which) {
  glue('
bake.{which}_{name} <- function(object, new_data, ...) {{
  <baking actions here>
  as_tibble(new_data)
}}

')
}

create_print_method <- function(name, which) {
  glue('
print.{which}_{name} <-
  function(x, width = max(20, options()$width - 30), ...) {{
    cat("<describe action here> ", sep = "")
    printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }}

')
}

create_tidy_method <- function(name, which) {
  glue("
#' @rdname {which}_{name}
#' @param x A `{which}_{name}` object.
#' @export
tidy.{which}_{name} <- function(x, ...) {{
  if (is_trained(x)) {{
    res <-
    <action here>
  }} else {{
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }}
  res$id <- x$id
  res
}}
  ")
}
