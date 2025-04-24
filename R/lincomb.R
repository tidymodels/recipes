#' Linear combination filter
#'
#' `step_lincomb()` creates a *specification* of a recipe step that will
#' potentially remove numeric variables that have exact linear combinations
#' between them.
#'
#' @inheritParams step_center
#' @param max_steps The number of times to apply the algorithm.
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
#' @template step-return
#' @template filter-steps
#' @family variable filter steps
#' @author Max Kuhn, Kirk Mettler, and Jed Wing
#' @export
#'
#' @details
#'
#' This step finds exact linear combinations between two or more variables and
#' recommends which column(s) should be removed to resolve the issue. This
#' algorithm may need to be applied multiple times (as defined by `max_steps`).
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass$new_1 <- with(
#'   biomass,
#'   .1 * carbon - .2 * hydrogen + .6 * sulfur
#' )
#' biomass$new_2 <- with(
#'   biomass,
#'   .5 * carbon - .2 * oxygen + .6 * nitrogen
#' )
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
#'   sulfur + new_1 + new_2,
#' data = biomass_tr
#' )
#'
#' lincomb_filter <- rec |>
#'   step_lincomb(all_numeric_predictors())
#'
#' lincomb_filter_trained <- prep(lincomb_filter, training = biomass_tr)
#' lincomb_filter_trained
#'
#' tidy(lincomb_filter, number = 1)
#' tidy(lincomb_filter_trained, number = 1)
step_lincomb <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    max_steps = 5,
    removals = NULL,
    skip = FALSE,
    id = rand_id("lincomb")
  ) {
    add_step(
      recipe,
      step_lincomb_new(
        terms = enquos(...),
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
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_whole(x$max_steps, arg = "max_steps", min = 1)

  filter <- iter_lc_rm(
    x = training[, col_names],
    max_steps = x$max_steps
  )

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
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_lincomb <-
  function(x, width = max(20, options()$width - 36), ...) {
    if (x$trained) {
      title <- "Linear combination filter removed "
    } else {
      title <- "Linear combination filter on "
    }
    print_step(x$removals, x$terms, x$trained, title, width)
    invisible(x)
  }

recommend_rm <- function(x, eps = 1e-6, ...) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  qr_decomp <- qr(x)
  qr_decomp_R <- qr.R(qr_decomp) # extract R matrix
  num_cols <- ncol(qr_decomp_R) # number of columns in R
  rank <- qr_decomp$rank # number of independent columns
  pivot <- qr_decomp$pivot # get the pivot vector

  if (is.null(num_cols) || rank == num_cols) {
    rm_list <- character(0) # there are no linear combinations
  } else {
    p1 <- seq_len(rank)
    X <- qr_decomp_R[p1, p1] # extract the independent columns
    Y <- qr_decomp_R[p1, -p1, drop = FALSE] # extract the dependent columns
    b <- qr(X) # factor the independent columns
    b <- qr.coef(b, Y) # get regression coefficients of
    # the dependent columns
    b[abs(b) < eps] <- 0 # zap small values

    # generate a list with one element for each dependent column
    combos <- lapply(
      seq_len(ncol(Y)),
      function(i) {
        c(pivot[rank + i], pivot[which(b[, i] != 0)])
      }
    )
    rm_list <- unlist(
      lapply(combos, function(x) {
        x[1]
      })
    )
    rm_list <- colnames(x)[rm_list]
  }
  rm_list
}

iter_lc_rm <- function(x, max_steps = 10, verbose = FALSE) {
  if (ncol(x) == 0L) {
    # Empty selection
    return(character())
  }

  orig_names <- colnames(x)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  # converting to matrix may alter column names
  name_df <- data.frame(
    orig = orig_names,
    current = colnames(x),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(max_steps)) {
    if (verbose) {
      cat(i)
    }
    if (i == max_steps) {
      break()
    }
    lcs <- recommend_rm(x)
    if (length(lcs) == 0) {
      break()
    } else {
      if (verbose) {
        cat(" removing", length(lcs), "\n")
      }
      x <- x[, !(colnames(x) %in% lcs), drop = FALSE]
    }
  }
  if (verbose) {
    cat("\n")
  }
  name_df <- name_df[!(name_df$current %in% colnames(x)), ]
  name_df$orig
}

#' @rdname tidy.recipe
#' @export
tidy.step_lincomb <- tidy_filter
