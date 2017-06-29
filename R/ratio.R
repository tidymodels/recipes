#' Ratio Variable Creation
#'
#' \code{step_ratio} creates a a \emph{specification} of a recipe step that
#'   will create one or more ratios out of numeric variables.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which variables will
#'   be used in the \emph{numerator} of the ratio. When used with
#'   \code{denom_vars}, the dots indicates which variables are used in the
#'   \emph{denominator}. See \code{\link{selections}} for more details.
#' @param role For terms created by this step, what analysis role should
#'   they be assigned?. By default, the function assumes that the newly created
#'   ratios created by the original variables will be used as
#'   predictors in a model.
#' @param denom A call to \code{denom_vars} to specify which variables are
#'   used in the denominator that can include specific variable names
#'   separated by commas or different selectors (see
#'   \code{\link{selections}}).  If a column is included in both lists to be
#'   numerator and denominator, it will be removed from the listing.
#' @param naming A function that defines the naming convention for new ratio
#'   columns.
#' @param variables The column names used in the rations. This argument is
#'   not populated until \code{\link{prepare.recipe}} is executed.
#' @return \code{step_ratio} returns an object of class \code{step_ratio}.
#' @keywords datagen
#' @concept preprocessing
#' @export
#' @examples 
#' library(recipes)
#' data(biomass)
#' 
#' biomass$total <- apply(biomass[, 3:7], 1, sum)
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#' 
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + 
#'                     sulfur + total,
#'               data = biomass_tr)
#' 
#' ratio_recipe <- rec %>%
#'   # all predictors over total
#'   step_ratio(all_predictors(), denom = denom_vars(total)) %>%
#'   # get rid of the original predictors 
#'   step_rm(all_predictors(), -matches("_o_"))
#'   
#' 
#' ratio_recipe <- prepare(ratio_recipe, training = biomass_tr)
#' 
#' ratio_data <- bake(ratio_recipe, biomass_te)
#' ratio_data

step_ratio <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           denom = denom_vars(),
           naming = function(numer, denom)
             make.names(paste(numer, denom, sep = "_o_")),
           variables = NULL) {
    if (is_empty(denom))
      stop("Please supply at least one denominator variable specification. ",
           "See ?selections.", call. = FALSE)
    add_step(
      recipe,
      step_ratio_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        denom = denom,
        naming = naming,
        variables = variables
      )
    )
  }

step_ratio_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           denom = NULL,
           naming = NULL,
           variables = NULL
  ) {
    step(
      subclass = "ratio",
      terms = terms,
      role = role,
      trained = trained,
      denom = denom,
      naming = naming,
      variables = variables
    )
  }


#' @export
prepare.step_ratio <- function(x, training, info = NULL, ...) {
  col_names <- expand.grid(
    top = terms_select(x$terms, info = info),
    bottom = terms_select(x$denom, info = info),
    stringsAsFactors = FALSE
  )
  col_names <- col_names[!(col_names$top == col_names$bottom), ]
  
  if (nrow(col_names) == 0)
    stop("No variables were selected for making ratios", call. = FALSE)
  if (any(info$type[info$variable %in% col_names$top] != "numeric"))
    stop("The ratio variables should be numeric")
  if (any(info$type[info$variable %in% col_names$bottom] != "numeric"))
    stop("The ratio variables should be numeric")
  
  step_ratio_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    denom = x$denom,
    naming = x$naming,
    variables = col_names
  )
}

#' @export
bake.step_ratio <- function(object, newdata, ...) {
  res <- newdata[, object$variables$top] /
    newdata[, object$variables$bottom]
  colnames(res) <-
    apply(object$variables, 1, function(x)
      object$naming(x[1], x[2]))
  if (!is_tibble(res))
    res <- as_tibble(res)
  
  newdata <- cbind(newdata, res)
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}

print.step_ratio <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Ratios from ")
    if (x$trained) {
      vars <- c(unique(x$variables$top), unique(x$variables$bottom))
      cat(format_ch_vec(vars, width = width))
    } else
      cat(format_selectors(c(x$terms, x$denom), wdth = width))
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @export
#' @rdname step_ratio
denom_vars <- function(...) quos(...)
