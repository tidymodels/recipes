#' Down-Sample a Data Set Based on a Factor Variable
#'
#' `step_downsample` creates a *specification* of a recipe
#'  step that will remove rows of a data set to make the occurrence
#'  of levels in a specific factor level equal.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [selections()]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param ratio A numeric value for the ratio of the
#'  minority-to-majority frequencies. The default value (1) means
#'  that all other levels are sampled down to have the same
#'  frequency as the least occurring level. A value of 2 would mean
#'  that the majority levels will have (at most) (approximately)
#'  twice as many rows than the minority level.
#' @param target An integer that will be used to subsample. This
#'  should not be set by the user and will be populated by `prep`.
#' @param seed An integer that will be used as the seed when downsampling.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#' @details
#' Down-sampling is intended to be performed on the _training_ set
#'  alone. For this reason, the default is `skip = TRUE`. It is
#'  advisable to use `prep(recipe, retain = TRUE)` when preparing
#'  the recipe; in this way [juice()] can be used to obtain the
#'  down-sampled version of the data.
#'
#' If there are missing values in the factor variable that is used
#'  to define the sampling, missing data are selected at random in
#'  the same way that the other factor levels are sampled. Missing
#'  values are not used to determine the amount of data in the
#'  minority level
#'
#' For any data with factor levels occurring with the same
#'  frequency as the minority level, all data will be retained.
#'
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' Keep in mind that the location of down-sampling in the step
#'  may have effects. For example, if centering and scaling,
#'  it is not clear whether those operations should be conducted
#'  _before_ or _after_ rows are removed.
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' @keywords datagen
#' @concept preprocessing subsampling
#' @export
#' @examples
#' data(okc)
#'
#' sort(table(okc$diet, useNA = "always"))
#'
#' ds_rec <- recipe( ~ ., data = okc) %>%
#'   step_downsample(diet) %>%
#'   prep(training = okc, retain = TRUE)
#'
#' table(juice(ds_rec)$diet, useNA = "always")
#'
#' # since `skip` defaults to TRUE, baking the step has no effect
#' baked_okc <- bake(ds_rec, new_data = okc)
#' table(baked_okc$diet, useNA = "always")

step_downsample <-
  function(recipe, ...,  ratio = 1, role = NA, trained = FALSE,
           column = NULL, target = NA, skip = TRUE,
           seed = sample.int(10^5, 1), id = rand_id("downsample")) {

    add_step(recipe,
             step_downsample_new(
               terms = ellipse_check(...),
               ratio = ratio,
               role = role,
               trained = trained,
               column = column,
               target = target,
               skip = skip,
               seed = seed,
               id = id
             ))
  }

step_downsample_new <-
  function(terms, ratio, role, trained, column, target, skip, seed, id) {
    step(
      subclass = "downsample",
      terms = terms,
      ratio = ratio,
      role = role,
      trained = trained,
      column = column,
      target = target,
      skip = skip,
      id = id,
      seed = seed,
      id = id
    )
  }


#' @export
prep.step_downsample <- function(x, training, info = NULL, ...) {
  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1)
    stop("Please select a single factor variable.", call. = FALSE)
  if (!is.factor(training[[col_name]]))
    stop(col_name, " should be a factor variable.", call. = FALSE)

  obs_freq <- table(training[[col_name]])
  minority <- min(obs_freq)

  step_downsample_new(
    terms = x$terms,
    ratio = x$ratio,
    role = x$role,
    trained = TRUE,
    column = col_name,
    target = floor(minority * x$ratio),
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}


subsamp <- function(x, num) {
  n <- nrow(x)
  if (nrow(x) == num)
    out <- x
  else
    # downsampling is done without replacement
    out <- x[sample(1:n, min(num, n)), ]
  out
}

#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#' @importFrom withr with_seed
#' @export
bake.step_downsample <- function(object, new_data, ...) {
  if (any(is.na(new_data[[object$column]])))
    missing <- new_data[is.na(new_data[[object$column]]),]
  else
    missing <- NULL
  split_up <- split(new_data, new_data[[object$column]])

  # Downsample with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      new_data <- map_dfr(split_up, subsamp, num = object$target)
      if (!is.null(missing)) {
        new_data <- bind_rows(new_data, subsamp(missing, object$target))
      }
    }
  )
  
  as_tibble(new_data)
}


print.step_downsample <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("Down-sampling based on ", sep = "")
    printer(x$column, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_downsample
#' @param x A `step_downsample` object.
#' @export
tidy.step_downsample <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$column)
  }
  else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = unname(term_names))
  }
  res$id <- x$id
  res
}
