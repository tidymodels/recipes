#' Up-Sample a Data Set Based on a Factor Variable
#' 
#' `step_upsample` creates a *specification* of a recipe step that
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
#' @param ratio A numeric value for the ratio of the 
#'  majority-to-minority frequencies. The default value (1) means
#'  that all other levels are sampled up to have the same
#'  frequency as the most occurring level. A value of 0.5 would mean
#'  that the minority levels will have (at most) (approximately)
#'  half as many rows than the majority level. 
#' @param target An integer that will be used to subsample. This
#'  should not be set by the user and will be populated by `prep`.
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
#' @keywords datagen
#' @concept preprocessing subsampling
#' @export
#' @examples
#' data(okc)
#' 
#' orig <- table(okc$diet, useNA = "always")
#' 
#' sort(orig, decreasing = TRUE)
#' 
#' up_rec <- recipe( ~ ., data = okc) %>%
#'   # Bring the minority levels up to about 200 each
#'   # 200/16562 is approx 0.0121
#'   step_upsample(diet, ratio = 0.0121) %>%
#'   prep(training = okc, retain = TRUE)
#' 
#' training <- table(juice(up_rec)$diet, useNA = "always")
#' 
#' # Since `skip` defaults to TRUE, baking the step has no effect
#' baked_okc <- bake(up_rec, newdata = okc)
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
  function(recipe, ...,  ratio = 1, role = NA, trained = FALSE, 
           column = NULL, target = NA, skip = TRUE) {
    add_step(recipe,
             step_upsample_new(
               terms = ellipse_check(...),
               ratio = ratio,
               role = role,
               trained = trained,
               column = column,
               target = target,
               skip = skip
             ))
  }

step_upsample_new <-
  function(terms = NULL, ratio = NA, role = NA, trained = FALSE, 
           column = NULL, target = NA, skip = FALSE) {
    step(
      subclass = "upsample",
      terms = terms,
      ratio = ratio,
      role = role,
      trained = trained,
      column = column,
      target = target,
      skip = skip
    )
  }


#' @export
prep.step_upsample <- function(x, training, info = NULL, ...) {
  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1)
    stop("Please select a single factor variable.", call. = FALSE)
  if (!is.factor(training[[col_name]]))
    stop(col_name, " should be a factor variable.", call. = FALSE)
  
  
  obs_freq <- table(training[[col_name]])
  majority <- max(obs_freq)
  
  step_upsample_new(
    terms = x$terms,
    ratio = x$ratio,
    role = x$role,
    trained = TRUE,
    column = col_name,
    target = floor(majority * x$ratio),
    skip = x$skip
  )
}


supsamp <- function(x, num) {
  n <- nrow(x)
  if (nrow(x) == num)
    out <- x
  else
    out <- x[sample(1:n, max(num, n), replace = TRUE), ]
  out
}

#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#' @export
bake.step_upsample <- function(object, newdata, ...) {
  if (any(is.na(newdata[[object$column]])))
    missing <- newdata[is.na(newdata[[object$column]]),]
  else
    missing <- NULL
  split_up <- split(newdata, newdata[[object$column]])
  
  newdata <- map_dfr(split_up, supsamp, num = object$target)
  if (!is.null(missing))
    newdata <- bind_rows(newdata, supsamp(missing, object$target))
  as_tibble(newdata)
}


print.step_upsample <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("Up-sampling based on ", sep = "")
    printer(x$column, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_upsample
#' @param x A `step_upsample` object.
tidy.step_upsample <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$column)
  }
  else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = unname(term_names))
  }
  res
}
