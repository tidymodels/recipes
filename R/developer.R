#' @name developer-functions
#'
#' @title Developer functions for creating recipes steps
#'
#' @description
#'
#' This page provides a comprehensive list of the functions that are exported
#' for creating recipes steps, and how to use them.
#'
#' @details
#'
#' # Creating steps
#'
#' [add_step()] and [add_check()] are required to be used when creating a new
#' step. It should be the other function call in your steps and should have the
#' following format:
#'
#' ```r
#' step_example <- function(recipe,
#'                          ...,
#'                          role = NA,
#'                          trained = FALSE,
#'                          skip = FALSE,
#'                          id = rand_id("example")) {
#'   add_step(
#'     recipe,
#'     step_example_new(
#'       terms = enquos(...),
#'       role = role,
#'       trained = trained,
#'       skip = skip,
#'       id = id
#'     )
#'   )
#' }
#' ```
#'
#' [step()] and [check()] are used within the `step_*_new()` function that you
#' use in your new step. It will be used in the following way:
#'
#' ```r
#' step_example_new <- function(terms, role, trained, skip, id) {
#'   step(
#'     subclass = "example",
#'     terms = terms,
#'     role = role,
#'     trained = trained,
#'     skip = skip,
#'     id = id
#'   )
#' }
#' ```
#'
#' # Interacting with recipe objects
#'
#' [detect_step()] returns a logical indicator to determine if a given step or
#' check is included in a recipe.
#'
#' [fully_trained()] returns a logical indicator if the recipe is fully trained.
#'
NULL
