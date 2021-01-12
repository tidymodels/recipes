#' Applies a recipe for each group
#'
#' Applies a recipe for each group specified in the `groups`
#' parameter.
#'
#' @note It's better to use this step as the first step of
#'  your pipeline, or before creating any new variables as
#'  it might be harder to correctly specify the grouped recipe
#'  otherwise.
#'
#' @param groups variables or computations to group by
#'   when preparing the recipe.
#' @param grouped_recipe a recipe specification to apply
#'   for each group.
#' @inheritParams step_center
#'
#' @examples
#' df <- tibble::tibble(
#'   g = sample(letters[1:3], size = 100, replace = TRUE),
#'   x = runif(100),
#'   y = rnorm(100)
#' )
#'
#' grouped_rec <- recipes::recipe(y ~ ., data = df) %>%
#'   recipes::step_center(x, y)
#'
#' rec <- recipes::recipe(y ~ ., df) %>%
#'   step_group(g, grouped_rec)
#'
#' rec <- recipes::prep(rec, df)
#'
#' out <- bake(rec, df)
#'
#' @export
step_group_apply <- function(
  recipe,
  groups,
  grouped_recipe,
  trained_recipes = NA,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("group_apply")
) {

  recipes::add_step(
    recipe,
    step_group_apply_new(
      groups = rlang::enquos(groups),
      grouped_recipe = grouped_recipe,
      trained_recipes = trained_recipes,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_group_apply_new <- function(groups, grouped_recipe, trained_recipes,
                                 role, trained, skip, id) {
  recipes::step(
    subclass = "group_apply",
    groups = groups,
    grouped_recipe = grouped_recipe,
    trained_recipes = trained_recipes,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_group_apply <- function(x, training, info = NULL, ...) {
  trained_recipes <- training %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
    dplyr::group_nest(!!!x$groups, keep = TRUE) %>%
    dplyr::mutate(recipe = purrr::map(data, ~prep(x$grouped_recipe, .x, string_as_factors = FALSE))) %>%
    dplyr::select(-data)
  step_group_apply_new(
    groups = x$groups,
    grouped_recipe = x$grouped_recipe,
    trained_recipes = trained_recipes,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_group_apply <- function(object, new_data, ...) {

  tmp <- new_data %>%
    dplyr::group_nest(!!!object$groups, keep = TRUE) %>%
    dplyr::left_join(
      object$trained_recipes,
      by = names(dplyr::select(object$trained_recipes, -recipe))
    )

  purrr::map2_dfr(tmp$data, tmp$recipe, ~recipes::bake(.y, .x))
}
