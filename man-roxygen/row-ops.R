#' @section Row Filtering:
#'
#' This step can entirely remove observations (rows of data), which can have
#' unintended and/or problematic consequences when applying the step to new
#' data later via [bake.recipe()]. Consider whether `skip = TRUE` or
#' `skip = FALSE` is more appropriate in any given use case. In most instances
#' that affect the rows of the data being predicted, this step probably should
#' not be applied at all; instead, execute operations like this outside and
#' before starting a preprocessing [recipe()].
