#' recipes: A package for computing and preprocessing design matrices.
#'
#' The `recipes` package can be used to create design matrices for modeling
#'   and to conduct preprocessing of variables. It is meant to be a more
#'   extensive framework that R's formula method. Some differences between
#'   simple formula methods and recipes are that
#' \enumerate{
#' \item Variables can have arbitrary roles in the analysis beyond predictors
#'  and outcomes.
#' \item A recipe consists of one or more steps that define actions on the
#'  variables.
#' \item Recipes can be defined sequentially using pipes as well as being
#'  modifiable and extensible.
#' }
#'
#'
#' @section Basic Functions:
#' The three main functions are [recipe()], [prep()],
#'   and [bake()].
#'
#' [recipe()] defines the operations on the data and the associated
#'   roles. Once the preprocessing steps are defined, any parameters are
#'   estimated using [prep()]. Once the data are ready for
#'   transformation, the [bake()] function applies the operations.
#'
#' @section Step Functions:
#' These functions are used to add new actions to the recipe and have the
#'   naming convention `"step_action"`. For example,
#'   [step_center()] centers the data to have a zero mean and
#'   [step_dummy()] is used to create dummy variables.
#' @docType package
#' @name recipes
NULL
