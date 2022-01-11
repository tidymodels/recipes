#' Down-Sample a Data Set Based on a Factor Variable
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `step_downsample` is now available as `themis::step_downsample()`. This
#'  function creates a *specification* of a recipe step that will remove
#'  rows of a data set to make the occurrence of levels in a specific factor
#'  level equal.
#'
#' @inheritParams step_center
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param under_ratio A numeric value for the ratio of the
#'  minority-to-majority frequencies. The default value (1) means
#'  that all other levels are sampled down to have the same
#'  frequency as the least occurring level. A value of 2 would mean
#'  that the majority levels will have (at most) (approximately)
#'  twice as many rows than the minority level.
#' @param ratio Deprecated argument; same as `under_ratio`
#' @param target An integer that will be used to subsample. This
#'  should not be set by the user and will be populated by `prep`.
#' @param seed An integer that will be used as the seed when downsampling.
#' @template step-return
#' @details
#' Down-sampling is intended to be performed on the _training_ set alone. For
#'  this reason, the default is `skip = TRUE`. It is advisable to use
#'  `prep(recipe, retain = TRUE)` when preparing the recipe; in this way
#'  `bake(object, new_data = NULL)` can be used to obtain the down-sampled
#'  version of the data.
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
#' All columns in the data are sampled and returned by [bake()].
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
#' @keywords internal
#' @export
step_downsample <-
  function(recipe, ...,  under_ratio = 1, ratio = NA, role = NA, trained = FALSE,
           column = NULL, target = NA, skip = TRUE,
           seed = sample.int(10^5, 1), id = rand_id("downsample")) {

    lifecycle::deprecate_stop("0.1.13",
                              "recipes::step_downsample()",
                              "themis::step_downsample()")
  }

