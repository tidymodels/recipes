#' Linear Combination Filter
#'
#' `step_lincomb` creates a *specification* of a recipe
#'  step that will potentially remove numeric variables that have
#'  linear combinations between them.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param max_steps A value.
#' @param removals A character string that contains the names of
#'  columns that should be removed. These values are not determined
#'  until [prep.recipe()] is called.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be removed.
#' @keywords datagen
#' @concept preprocessing
#' @concept variable_filters
#' @author Max Kuhn, Kirk Mettler, and Jed Wing
#' @export
#'
#' @details This step finds exact linear combinations between two
#'  or more variables and recommends which column(s) should be
#'  removed to resolve the issue. This algorithm may need to be
#'  applied multiple times (as defined by `max_steps`).
#' @examples
#' library(modeldata)
#' data(biomass)
#'
#' biomass$new_1 <- with(biomass,
#'                       .1*carbon - .2*hydrogen + .6*sulfur)
#' biomass$new_2 <- with(biomass,
#'                       .5*carbon - .2*oxygen + .6*nitrogen)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
#'                 sulfur + new_1 + new_2,
#'               data = biomass_tr)
#'
#' lincomb_filter <- rec %>%
#'   step_lincomb(all_predictors())
#'
#' lincomb_filter_trained <- prep(lincomb_filter, training = biomass_tr)
#' lincomb_filter_trained
#'
#' tidy(lincomb_filter, number = 1)
#' tidy(lincomb_filter_trained, number = 1)
#' @seealso [step_nzv()][step_corr()]
#'   [recipe()] [prep.recipe()]
#'   [bake.recipe()]

step_lincomb <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           max_steps = 5,
           removals = NULL,
           skip = FALSE,
           id = rand_id("lincomb")) {
    add_step(
      recipe,
      step_lincomb_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        max_steps = max_steps,
        removals = removals,
        skip = skip,
        id = id
      )
    )
  }

step_lincomb_new <-
  function(terms, role, trained, max_steps, removals, skip, id) {
    step(
      subclass = "lincomb",
      terms = terms,
      role = role,
      trained = trained,
      max_steps = max_steps,
      removals = removals,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lincomb <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names])

  filter <- iter_lc_rm(x = training[, col_names],
                       max_steps = x$max_steps)

  step_lincomb_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    max_steps = x$max_steps,
    removals = filter,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_lincomb <- function(object, new_data, ...) {
  if (length(object$removals) > 0)
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  as_tibble(new_data)
}

print.step_lincomb <-
  function(x,  width = max(20, options()$width - 36), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Linear combination filter removed ")
        cat(format_ch_vec(x$removals, width = width))
      } else
        cat("Linear combination filter removed no terms")
    } else {
      cat("Linear combination filter on ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }


recommend_rm <- function(x, eps  = 1e-6, ...) {
  if (!is.matrix(x))
    x <- as.matrix(x)
  if (is.null(colnames(x)))
    rlang::abort("`x` should have column names")

  qr_decomp <- qr(x)
  qr_decomp_R <- qr.R(qr_decomp)           # extract R matrix
  num_cols <- ncol(qr_decomp_R)            # number of columns in R
  rank <- qr_decomp$rank                   # number of independent columns
  pivot <- qr_decomp$pivot                 # get the pivot vector

  if (is.null(num_cols) || rank == num_cols) {
    rm_list <- character(0)                 # there are no linear combinations
  } else {
    p1 <- 1:rank
    X <- qr_decomp_R[p1, p1]                # extract the independent columns
    Y <- qr_decomp_R[p1, -p1, drop = FALSE] # extract the dependent columns
    b <- qr(X)                              # factor the independent columns
    b <- qr.coef(b, Y)                      # get regression coefficients of
                                            # the dependent columns
    b[abs(b) < eps] <- 0                    # zap small values

    # generate a list with one element for each dependent column
    combos <- lapply(1:ncol(Y),
                     function(i)
                       c(pivot[rank + i], pivot[which(b[, i] != 0)]))
    rm_list <- unlist(lapply(combos, function(x)
      x[1]))
    rm_list <- colnames(x)[rm_list]
  }
  rm_list
}

iter_lc_rm <- function(x,
                       max_steps = 10,
                       verbose = FALSE) {
  if (is.null(colnames(x)))
    rlang::abort("`x` should have column names")

  orig_names <- colnames(x)
  if (!is.matrix(x))
    x <- as.matrix(x)

  # converting to matrix may alter column names
  name_df <- data.frame(orig = orig_names,
                        current = colnames(x),
                        stringsAsFactors = FALSE)

  for (i in 1:max_steps) {
    if (verbose)
      cat(i)
    if (i == max_steps)
      break ()
    lcs <- recommend_rm(x)
    if (length(lcs) == 0)
      break ()
    else {
      if (verbose)
        cat(" removing", length(lcs), "\n")
      x <- x[, !(colnames(x) %in% lcs)]
    }
  }
  if (verbose)
    cat("\n")
  name_df <- name_df[!(name_df$current %in% colnames(x)), ]
  name_df$orig
}


#' @rdname step_lincomb
#' @param x A `step_lincomb` object.
#' @export
tidy.step_lincomb <- tidy_filter
