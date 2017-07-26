#' Create Interaction Variables
#'
#' \code{step_interact} creates a \emph{specification} of a recipe step that
#'   will create new columns that are interaction terms between two or more
#'   variables.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param terms A traditional R formula that contains interaction terms.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned?. By default, the function assumes that the new columns
#'   created from the original variables will be used as predictors in a model.
#' @param objects A list of \code{terms} objects for each individual interation.
#' @param sep A character value used to delinate variables in an interaction
#'   (e.g. \code{var1_x_var2} instead of the more traditional \code{var1:var2}).
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
#' @details \code{step_interact} can create interactions between variables. It
#'   is primarily intended for \bold{numeric data}; categorical variables
#'   should probably be converted to dummy variables using
#'   \code{\link{step_dummy}} prior to being used for interactions.
#'
#' Unlike other step functions, the \code{terms} argument should be a
#'   traditional R model formula but should contain no inline functions (e.g.
#'   \code{log}). For example, for predictors \code{A}, \code{B}, and \code{C},
#'   a formula such as \code{~A:B:C} can be used to make a three way
#'   interaction between the variables. If the formula contains terms other
#'   than interactions (e.g. \code{(A+B+C)^3}) only the interaction terms are
#'   retained for the design matrix.
#'
#' The separator between the variables defaults to "\code{_x_}" so that the
#'   three way interaction shown previously would generate a column named
#'   \code{A_x_B_x_C}. This can be changed using the \code{sep} argument.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' int_mod_1 <- rec %>%
#'   step_interact(terms = ~ carbon:hydrogen)
#'
#' int_mod_2 <- int_mod_1 %>%
#'   step_interact(terms = ~ (oxygen + nitrogen + sulfur)^3)
#'
#' int_mod_1 <- prep(int_mod_1, training = biomass_tr)
#' int_mod_2 <- prep(int_mod_2, training = biomass_tr)
#'
#' dat_1 <- bake(int_mod_1, biomass_te)
#' dat_2 <- bake(int_mod_2, biomass_te)
#'
#' names(dat_1)
#' names(dat_2)

step_interact <-
  function(recipe,
           terms,
           role = "predictor",
           trained = FALSE,
           objects = NULL,
           sep = "_x_") {
    add_step(
      recipe,
      step_interact_new(
        terms = terms,
        trained = trained,
        role = role,
        objects = objects,
        sep = sep
      )
    )
  }

## Initializes a new object
step_interact_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           objects = NULL,
           sep = NULL) {
    step(
      subclass = "interact",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      sep = sep
    )
  }


## The idea is to save a bunch of x-factor interaction terms instead of
## one large set of collected terms.
#' @export
prep.step_interact <- function(x, training, info = NULL, ...) {
  ## First, find the interaction terms based on the given formula
  int_terms <- get_term_names(x$terms, vnames = colnames(training))
  
  ## Check to see if any variables are non-numeric and issue a warning
  ## if that is the case
  vars <-
    unique(unlist(lapply(make_new_formula(int_terms), all.vars)))
  var_check <- info[info$variable %in% vars, ]
  if (any(var_check$type == "nominal"))
    warning(
      "Categorical variables used in `step_interact` should probably be ",
      "avoided;  This can lead to differences in dummy variable values that ",
      "are produced by `step_dummy`."
    )
  
  ## For each interaction, create a new formula that has main effects
  ## and only the interaction of choice (e.g. `a+b+c+a:b:c`)
  int_forms <- make_new_formula(int_terms)
  
  ## Generate a standard R `terms` object from these short formulas and
  ## save to make future interactions
  int_terms <- make_small_terms(int_forms, training)
  
  step_interact_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = int_terms,
    sep = x$sep
  )
}


#' @export
bake.step_interact <- function(object, newdata, ...) {
  ## `na.action` cannot be passed to `model.matrix` but we
  ## can change it globally for a bit
  
  old_opt <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = old_opt))
  
  ## Create low level model matrices then remove the non-interaction terms.
  res <- lapply(object$object, model.matrix, data = newdata)
  options(na.action = old_opt)
  on.exit(expr = NULL)
  
  res <-
    lapply(res, function(x)
      x[, grepl(":", colnames(x)), drop = FALSE])
  ncols <- vapply(res, ncol, c(int = 1L))
  out <- matrix(NA, nrow = nrow(newdata), ncol = sum(ncols))
  strt <- 1
  for (i in seq_along(ncols)) {
    cols <- (strt):(strt + ncols[i] - 1)
    out[, cols] <- res[[i]]
    strt <- max(cols) + 1
  }
  colnames(out) <-
    gsub(":", object$sep, unlist(lapply(res, colnames)))
  newdata <- cbind(newdata, as_tibble(out))
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}

## This uses the highest level of interactions
x_fac_int <- function(x)
  as.formula(
    paste0("~",
           paste0(x, collapse = "+"),
           "+",
           paste0(x, collapse = ":")
    )
  )

make_new_formula <- function(x) {
  splitup <- strsplit(x, ":")
  lapply(splitup, x_fac_int)
}


#' @importFrom stats model.matrix

## Given a standard model formula and some data, get the
## term expansion (without `.`s). This returns the factor
## names and would not expand dummy variables.
get_term_names <- function(form, vnames) {
  ## We are going to cheat and make a small fake data set to
  ## effcicently get the full formula exapnsion from
  ## model.matrix (devoid of factor levels) and then
  ## pick off the interactions
  dat <- matrix(1, nrow = 5, ncol = length(vnames))
  colnames(dat) <- vnames
  nms <- colnames(model.matrix(form, data = as.data.frame(dat)))
  nms <- nms[nms != "(Intercept)"]
  nms <- grep(":", nms, value = TRUE)
  nms
}

#' @importFrom stats terms

## For a given data set and a list of formulas, generate the
## standard R `terms` objects
make_small_terms <- function(forms, dat) {
  lapply(forms, terms, data = dat)
}


print.step_interact <-
  function(x, width = max(20, options()$width - 27), ...) {
    cat("Interactions with ", sep = "")
    cat(as.character(x$terms)[-1])
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }
