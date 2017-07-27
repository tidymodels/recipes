#' Linear Combination Filter
#'
#' \code{step_lincomb} creates a \emph{specification} of a recipe step that
#'   will potentially remove numeric variables that have linear combinations
#'   between them.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param role Not used by this step since no new variables are created.
#' @param max_steps A value .
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until
#'   \code{\link{prep.recipe}} is called.
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @author Max Kuhn, Kirk Mettler, and Jed Wing
#' @export
#'
#' @details This step finds exact linear combinations between two or more
#'   variables and recommends which column(s) should be removed to resolve the
#'   issue. This algorithm may need to be applied multiple times (as defined
#'   by \code{max_steps}).
#' @examples
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
#' prep(lincomb_filter, training = biomass_tr)
#' @seealso \code{\link{step_nzv}}\code{\link{step_corr}}
#'   \code{\link{recipe}} \code{\link{prep.recipe}}
#'   \code{\link{bake.recipe}}

step_lincomb <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           max_steps = 5,
           removals = NULL) {
    add_step(
      recipe,
      step_lincomb_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        max_steps = max_steps,
        removals = removals
      )
    )
  }

step_lincomb_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           max_steps = NULL,
           removals = NULL) {
    step(
      subclass = "lincomb",
      terms = terms,
      role = role,
      trained = trained,
      max_steps = max_steps,
      removals = removals
    )
  }

#' @export
prep.step_lincomb <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  if (any(info$type[info$variable %in% col_names] != "numeric"))
    stop("All variables for mean imputation should be numeric")
  
  filter <- iter_lc_rm(x = training[, col_names],
                       max_steps = x$max_steps)
  
  step_lincomb_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    max_steps = x$max_steps,
    removals = filter
  )
}

#' @export
bake.step_lincomb <- function(object, newdata, ...) {
  if (length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  as_tibble(newdata)
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
      cat(format_selectors(x$terms, wdth = width))
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
    stop("`x` should have column names", call. = FALSE)
  
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
    stop("`x` should have column names", call. = FALSE)
  
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
