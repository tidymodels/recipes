#' Custom Transformation
#'
#' `step_custom_transformation` creates a *specification* of a (higher order)
#' recipe step that will make a transformation of the input data from (custom)
#' `prep` and `bake` helper functions.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details. The names of the
#'   selected variables will be stored in the `selected_vars` argument.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the function assumes that the new columns
#'   will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [bake.recipe()]? While all operations are baked when [prep.recipe()] is
#'   run, some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using `skip
#'   = TRUE` as it may affect the computations for subsequent operations.
#' @param prep_function A function. This is a helper function for the
#'   [prep.recipe()] method. It will be invoked, when the recipe is 'prepped'
#'   by [prep.recipe()]. The function MUST satisfy the following conditions:
#'   (1) the function must take an argument `x`: input data with only the
#'   selected variables (`selected_vars`), (2) the function MUST return the
#'   (required) estimated parameters from a training set that can be later
#'   applied to other data sets. This output can be of any appropriate type and
#'   shape. Leave `prep_function` as NULL, if the preparation of new data sets
#'   does not depend on parameters learned on the training set.
#' @param prep_options A list with (any) additional arguments for the prep
#'   helper function call EXCEPT for the `x` argument. Leave as NULL, if no
#'   `prep_function` is given.
#' @param prep_output Output from prep helper (`prep_function`) function call
#'   consisting of the estimated parameters from the training set, that will be
#'   applied to other data sets. Results are not computed until [prep.recipe()]
#'   is called.
#' @param bake_function A function. This is a helper function for the 'bake'
#'   method. It will be invoked, when the recipe is 'baked' by `bake.recipe()`.
#'   The function MUST satisfy the following conditions: (1) the function must
#'   take an argument `x`: data set with only the selected variables
#'   (`selected_vars`) from the new data set, (2) if the preparation of new data
#'   sets depends on parameters learned on the train set, the function must take
#'   the argument `prep_output`: the output from the prep helper fct
#'   (`prep_function`), (3) the output from from the function should be the
#'   transformed variables. The output must be of a type and shape, that allows
#'   it to be binded column wise to the new data set after converting it to a
#'   `tibble`.
#' @param bake_options A list with (any) arguments for the bake helper function
#'   call EXCEPT for the `x` and `prep_output` arguments.
#' @param bake_how A character. How should the transformed variables be appended
#'   to the new data set? Choose from options (1) `bind_cols`: simply bind the
#'   transformed variables to the new data set or (2) `replace`: replace the
#'   selected variables (`selected vars`) from the new data set with the
#'   transformed variables.
#' @param selected_vars A character string that contains the names of the
#'   selected variables. These values are not determined until [prep.recipe()]
#'   is called.
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a `tibble` with
#'   columns `terms` (the selectors or variables selected).
#'
#' @keywords datagen
#' @concept preprocessing
#'
#' @importFrom methods formalArgs
#' @importFrom purrr safely
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyselect)
#' library(purrr)
#' library(tibble)
#'
#' # generate data.
#' df <- tibble(a = rnorm(100),
#'              b = rnorm(100),
#'              c = rnorm(100))
#'
#' # define prep helper function, that computes means and standard deviations
#' # for all variables in a data set.
#' compute_means_sd <- function(x, na.rm = FALSE, trim = 0) {
#'
#'   map(x, ~ list(mean = mean(.x, na.rm = na.rm, trim = trim),
#'                 sd = sd(.x)))
#'
#' }
#'
#' # define bake helper function, that subtracts k means from the variable and
#' # then divides by the standard deviation.
#' center_scale <- function(x, prep_output, k) {
#'
#'   newdata <- select(x, names(prep_output))
#'
#'   map2(.x = newdata,
#'        .y = prep_output,
#'        ~ (.x - k * .y$mean) / .y$sd)
#' }
#'
#' # create recipe.
#' rec <- recipe(df) %>%
#'   step_custom_transformation(b, c,
#'                              prep_function = compute_means_sd,
#'                              prep_options = list(na.rm = TRUE, trim = 0.05),
#'                              bake_function = center_scale,
#'                              bake_options = list(k = 2),
#'                              bake_how = "bind_cols")
#'
#' # prep recipe.
#' rec_prep <- prep(rec)
#'
#' # bake recipe.
#' rec_baked <- bake(rec_prep, df)
#' rec_baked
#'
#' # inspect output.
#' rec
#' rec_baked
#' tidy(rec)
#' tidy(rec, 1)
#' tidy(rec_prep)
#' tidy(rec_prep, 1)
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]
step_custom_transformation <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           prep_function = NULL,
           prep_options = NULL,
           prep_output = NULL,
           bake_function = NULL,
           bake_options = NULL,
           bake_how = "bind_cols",
           selected_vars = NULL,
           skip = FALSE,
           id = rand_id("custom_transformation")) {

    #### check inputs.
    if (is.null(bake_function)) {
      stop("No bake helper function ('bake_function') has been set.")
    }

    # inputs for 'prep.recipe()'.
    if (!is.null(prep_function) && !is.function(prep_function)) {
      stop("'prep_function' must be a function.")
    }

    # check inputs.
    if (is.null(prep_function) && !is.null(prep_options)) {
      stop("Arguments for the prep helper function have been provided, but
           no prep helper function has been set.")
    }

    if (!is.null(prep_options) && !is.list(prep_options)) {
      stop("'prep_options' must be a list.")
    }

    if (!is.null(prep_function) && !("x" %in% formalArgs(prep_function))) {
      stop("The prep helper function - 'prep_function' - must take an 'x'
           argument, that should correspond to the selected variables
           subset of the training data.")
    }

    # inputs for 'bake.recipe()'.
    if (!is.function(bake_function)) {
      stop("'bake_function' must be a function.")
    }

    if (!is.null(bake_options) && !is.list(bake_options)) {
      stop("'bake_options' must be a list.")
    }

    if (!isTRUE(bake_how %in% c("bind_cols", "replace"))) {
      stop("Set 'bake_how' to either 'bind_cols' or 'replace'.")
    }

    if (!("x" %in% formalArgs(bake_function))) {
      stop("The bake helper function - 'bake_function' - must take an 'x'
           argument, that should correspond to the selected variables subset
           of the new data set on which the recipe step will be applied.")
    }

    if (!is.null(prep_function) &&
        !("prep_output" %in% formalArgs(bake_function))) {
      stop("A prep helper function ('prep_function') has been given, but the
            bake helper function - 'bake_function' - does not take
            a 'prep_output' argument.")
    }

    # add step.
    add_step(
      recipe,
      step_custom_transformation_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        prep_function = prep_function,
        prep_options = prep_options,
        prep_output = prep_output,
        bake_function = bake_function,
        bake_options = bake_options,
        bake_how = bake_how,
        selected_vars = selected_vars,
        skip = skip,
        id = id
      )
    )
    }

# constructor function.
step_custom_transformation_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           prep_function = NULL,
           prep_options = NULL,
           prep_output = prep_output,
           bake_function = NULL,
           bake_options = NULL,
           bake_how = "bind_cols",
           selected_vars = NULL,
           skip = FALSE,
           id = id) {
    step(
      subclass = "custom_transformation",
      terms = terms,
      role = role,
      trained = trained,
      prep_function = prep_function,
      prep_options = prep_options,
      prep_output = prep_output,
      bake_function = bake_function,
      bake_options = bake_options,
      bake_how = bake_how,
      selected_vars = selected_vars,
      skip = skip,
      id = id
    )
  }

# prepare step (/estimation procedure).
prep.step_custom_transformation <- function(x, training, info = NULL, ...) {

  # selected vars as character vector.
  selected_vars <- terms_select(x$terms, info = info)

  # if no prep helper function has been specified, do nothing. Execute the
  # prep helper function otherwise.
  if (!is.null(x$prep_function)) {

    #### prepare all arguments before calling the prep helper function.

    # add mandatory argument 'x'.
    args <- list(x = training[, selected_vars])

    # add additional arguments (if any).
    if (!is.null(x$prep_options)) {
      args <- append(args, x$prep_options)
    }

    # compute intermediate output from prep helper function.
    prep_output <- do.call(safely(x$prep_function), args)

    # handle errors (if any).
    if (!is.null(prep_output$error)) {
      cat("An error occured in the call to the prep helper function",
          "('prep_function'). See details below: ")
      stop(prep_output$error)
    }

    # otherwise continue with output from the function call.
    prep_output <- prep_output$result

  } else {

    # set output to NULL otherwise.
    prep_output <- NULL

  }

  step_custom_transformation_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    prep_function = x$prep_function,
    prep_options = x$prep_options,
    prep_output = prep_output,
    bake_function = x$bake_function,
    bake_options = x$bake_options,
    bake_how = x$bake_how,
    selected_vars = selected_vars,
    skip = x$skip,
    id = x$id
    )

  }

# bake step (/apply step).
bake.step_custom_transformation <- function(object, newdata, ...) {

  #### prepare arguments before calling the bake helper function.

  # add mandatory argument for 'x' - set to newdata.
  args <- list(x = newdata)

  # add intermediate output from the prep helper function.
  if (!is.null(object$prep_output)) {
    args <- append(args, list(prep_output = object$prep_output))
  }

  # add additional arguments (if any).
  if (!is.null(object$bake_options)) {
    args <- append(args, object$bake_options)
  }

  # invoke the bake helper function.
  bake_function_output <- do.call(object$bake_function, args) %>%
    # convert output to tibble.
    as_tibble()

  # append to input data the output from the bake helper function.
  output <- switch(object$bake_how,

                   # append output to input by binding columns.
                   "bind_cols" = {

                     # check_inputs.
                     if (nrow(bake_function_output) != nrow(newdata)) {
                       stop("There was a mismatch between the number of rows ",
                            "in the output from the bake helper function (",
                            nrow(bake_function_output),
                            ") and the number of rows of the input data (",
                            nrow(newdata), ").")
                     }

                     # bind output columns to input data.frame.
                     newdata %>%
                       as.tibble() %>%
                       bind_cols(bake_function_output)
                   },

                   # replace selected variables with output.
                   "replace" = {

                     # check_inputs.
                     if (!all(names(newdata) %in% object$selected_vars) &&
                         nrow(bake_function_output) != nrow(newdata)) {
                       stop("There was a mismatch between the ",
                            "number of rows in the output from ",
                            "the bake helper function (",
                            nrow(bake_function_output),
                            ") and the number of rows of the input data ",
                            "(", nrow(newdata), ").")
                     }

                     newdata %>%
                       as.tibble() %>%
                       # drop selected vars.
                       select(-c(object$selected_vars)) %>%
                       # bind output columns to input data.frame.
                       bind_cols(bake_function_output)

                   })

  # return output.
  output

}

print.step_custom_transformation <-
  function(x, width = max(20, options()$width - 30), ...) {

    cat("The following variables are used for computing" ,
        " transformations", ifelse(x$bake_how == "replace",
                                   "\n and dropped afterwards:\n ",
                                   ":\n "), sep = "")
    cat(format_selectors(x$terms, wdth = width))
    invisible(x)

  }

#' @rdname step_custom_transformation
#' @param x A `step_custom_transformation` object.
#' @export
tidy.step_custom_transformation <- function(x, ...) {

  res <- tibble(terms = sel2char(x$terms))

}
