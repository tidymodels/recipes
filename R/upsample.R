#' Up-Sample a Data Set Based on a Factor Variable
#'
#' @description
#' \if{html}{\figure{lifecycle-soft-deprecated.svg}{alt="lifecycle-soft-deprecated"}}
#'
#' `step_upsample` is now available as `themis::step_upsample()`. This
#'  function creates a *specification* of a recipe step that
#'  will replicate rows of a data set to make the occurrence of
#'  levels in a specific factor level equal.
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
#' @param over_ratio A numeric value for the ratio of the
#'  majority-to-minority frequencies. The default value (1) means
#'  that all other levels are sampled up to have the same
#'  frequency as the most occurring level. A value of 0.5 would mean
#'  that the minority levels will have (at most) (approximately)
#'  half as many rows than the majority level.
#' @param ratio Deprecated argument; same as `over_ratio`.
#' @param target An integer that will be used to subsample. This
#'  should not be set by the user and will be populated by `prep`.
#' @param seed An integer that will be used as the seed when upsampling.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#' @details
#' Up-sampling is intended to be performed on the _training_ set
#'  alone. For this reason, the default is `skip = TRUE`. It is
#'  advisable to use `prep(recipe, retain = TRUE)` when preparing
#'  the recipe; in this way [juice()] can be used to obtain the
#'  up-sampled version of the data.
#'
#' If there are missing values in the factor variable that is used
#'  to define the sampling, missing data are selected at random in
#'  the same way that the other factor levels are sampled. Missing
#'  values are not used to determine the amount of data in the
#'  majority level (see example below).
#'
#' For any data with factor levels occurring with the same
#'  frequency as the majority level, all data will be retained.
#'
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept subsampling
#' @export
#' @examples
#' library(modeldata)
#' data(okc)
#'
#' orig <- table(okc$diet, useNA = "always")
#'
#' sort(orig, decreasing = TRUE)
#'
#' up_rec <- recipe( ~ ., data = okc) %>%
#'   # Bring the minority levels up to about 200 each
#'   # 200/16562 is approx 0.0121
#'   step_upsample(diet, over_ratio = 0.0121) %>%
#'   prep(training = okc)
#'
#' training <- table(juice(up_rec)$diet, useNA = "always")
#'
#' # Since `skip` defaults to TRUE, baking the step has no effect
#' baked_okc <- bake(up_rec, new_data = okc)
#' baked <- table(baked_okc$diet, useNA = "always")
#'
#' # Note that if the original data contained more rows than the
#' # target n (= ratio * majority_n), the data are left alone:
#' data.frame(
#'   level = names(orig),
#'   orig_freq = as.vector(orig),
#'   train_freq = as.vector(training),
#'   baked_freq = as.vector(baked)
#' )

step_upsample <-
  function(recipe, ...,  over_ratio = 1, ratio = NA, role = NA, trained = FALSE,
           column = NULL, target = NA, skip = TRUE,
           seed = sample.int(10^5, 1),
           id = rand_id("upsample")) {

    lifecycle::deprecate_soft("0.1.13",
                              "recipes::step_upsample()",
                              "themis::step_upsample()")

    if (!is.na(ratio) & all(over_ratio != ratio)) {
      message(
        paste(
          "The `ratio` argument is now deprecated in favor of `over_ratio`.",
          "`ratio` will be removed in a subsequent version."
        )
      )
      if (!is.na(ratio)) {
        over_ratio <- ratio
      }
    }

    add_step(recipe,
             step_upsample_new(
               terms = ellipse_check(...),
               over_ratio = over_ratio,
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

step_upsample_new <-
  function(terms, over_ratio, ratio, role, trained, column, target, skip, seed, id) {
    step(
      subclass = "upsample",
      terms = terms,
      over_ratio = over_ratio,
      ratio = ratio,
      role = role,
      trained = trained,
      column = column,
      target = target,
      skip = skip,
      id = id,
      seed = seed
    )
  }


#' @export
prep.step_upsample <- function(x, training, info = NULL, ...) {
  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1)
    rlang::abort("Please select a single factor variable.")
  if (!is.factor(training[[col_name]]))
    rlang::abort(col_name, " should be a factor variable.")


  obs_freq <- table(training[[col_name]])
  majority <- max(obs_freq)

  step_upsample_new(
    terms = x$terms,
    ratio = x$ratio,
    over_ratio = x$over_ratio,
    role = x$role,
    trained = TRUE,
    column = col_name,
    target = floor(majority * x$over_ratio),
    skip = x$skip,
    id = x$id,
    seed = x$seed
  )
}


supsamp <- function(x, num) {
  n <- nrow(x)
  if (nrow(x) == num)
    out <- x
  else
    # upsampling is done with replacement
    out <- x[sample(1:n, max(num, n), replace = TRUE), ]
  out
}

#' @export
bake.step_upsample <- function(object, new_data, ...) {
  if (any(is.na(new_data[[object$column]])))
    missing <- new_data[is.na(new_data[[object$column]]),]
  else
    missing <- NULL
  split_up <- split(new_data, new_data[[object$column]])

  # Upsample with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      new_data <- map_dfr(split_up, supsamp, num = object$target)
      if (!is.null(missing)) {
        new_data <- bind_rows(new_data, supsamp(missing, object$target))
      }
    }
  )

  as_tibble(new_data)
}


print.step_upsample <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("Up-sampling based on ", sep = "")
    printer(x$column, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_upsample
#' @param x A `step_upsample` object.
#' @export
tidy.step_upsample <- function(x, ...) {
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



#' @rdname tunable.step
#' @export
tunable.step_upsample <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio")
    ),
    source = "recipe",
    component = "step_upsample",
    component_id = x$id
  )
}

