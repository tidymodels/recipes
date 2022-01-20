#' Up-Sample a Data Set Based on a Factor Variable
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `step_upsample` is now available as `themis::step_upsample()`. This
#'  function creates a *specification* of a recipe step that
#'  will replicate rows of a data set to make the occurrence of
#'  levels in a specific factor level equal.
#'
#' @inheritParams step_center
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
#' @template step-return
#' @details
#' Up-sampling is intended to be performed on the _training_ set alone. For
#'  this reason, the default is `skip = TRUE`. It is advisable to use
#'  `prep(recipe, retain = TRUE)` when preparing the recipe; in this way
#'  `bake(object, new_data = NULL)` can be used to obtain the up-sampled version
#'  of the data.
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
#' All columns in the data are sampled and returned by [bake()].
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' @keywords internal
#' @export
step_upsample <-
  function(recipe, ...,  over_ratio = 1, ratio = NA, role = NA, trained = FALSE,
           column = NULL, target = NA, skip = TRUE,
           seed = sample.int(10^5, 1),
           id = rand_id("upsample")) {

    lifecycle::deprecate_stop("0.1.13",
                              "recipes::step_upsample()",
                              "themis::step_upsample()")
  }
