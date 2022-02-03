#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = FALSE`.
#'
#' @section Row Filtering:
#'
#' This step can entirely remove observations (rows of data), which can have
#' unintended and/or problematic consequences when applying the step to new
#' data later via [bake()]. Consider whether `skip = TRUE` or
#' `skip = FALSE` is more appropriate in any given use case. In most instances
#' that affect the rows of the data being predicted, this step probably should
#' not be applied at all; instead, execute operations like this outside and
#' before starting a preprocessing [recipe()].
