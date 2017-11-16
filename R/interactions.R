#' Create Interaction Variables
#'
#' `step_interact` creates a *specification* of a recipe
#'  step that will create new columns that are interaction terms
#'  between two or more variables.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param terms A traditional R formula that contains interaction
#'  terms. This can include `.` and selectors. 
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created from the original variables will be
#'  used as predictors in a model.
#' @param objects A list of `terms` objects for each
#'  individual interaction.
#' @param sep A character value used to delineate variables in an
#'  interaction (e.g. `var1_x_var2` instead of the more
#'  traditional `var1:var2`).
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the interaction effects.
#' @keywords datagen
#' @concept preprocessing model_specification
#' @export
#' @details `step_interact` can create interactions between
#'  variables. It is primarily intended for **numeric data**;
#'  categorical variables should probably be converted to dummy
#'  variables using [step_dummy()] prior to being used for
#'  interactions.
#'
#' Unlike other step functions, the `terms` argument should
#'  be a traditional R model formula but should contain no inline
#'  functions (e.g. `log`). For example, for predictors
#'  `A`, `B`, and `C`, a formula such as
#'  `~A:B:C` can be used to make a three way interaction
#'  between the variables. If the formula contains terms other than
#'  interactions (e.g. `(A+B+C)^3`) only the interaction terms
#'  are retained for the design matrix.
#'
#' The separator between the variables defaults to "`_x_`" so
#'  that the three way interaction shown previously would generate a
#'  column named `A_x_B_x_C`. This can be changed using the
#'  `sep` argument.
#'  
#' When dummy variables are created and are used in interactions,
#'  selectors can help specify the interactions succinctly. For
#'  example, suppose a factor column `X` gets converted to dummy
#'  variables `x_2`, `x_3`, ..., `x_6` using [step_dummy()]. If
#'  you wanted an interaction with numeric column `z`, you could
#'  create a set of specific interaction effects (e.g. 
#'  `x_2:z + x_3:z` and so on) or you could use 
#'  `starts_with("z_"):z`. When [prep()] evaluates this step,
#'  `starts_with("z_")` resolves to `(x_2 + x_3 + x_4 + x_5 + x6)`
#'  so that the formula is now `(x_2 + x_3 + x_4 + x_5 + x6):z` and
#'  all two-way interactions are created. 

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
#' int_mod_2 <- rec %>%
#'   step_interact(terms = ~ (matches("gen$") + sulfur)^2)
#'
#' int_mod_1 <- prep(int_mod_1, training = biomass_tr)
#' int_mod_2 <- prep(int_mod_2, training = biomass_tr)
#'
#' dat_1 <- bake(int_mod_1, biomass_te)
#' dat_2 <- bake(int_mod_2, biomass_te)
#'
#' names(dat_1)
#' names(dat_2)
#'
#' tidy(int_mod_1, number = 1)
#' tidy(int_mod_2, number = 1)

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
  # Identify any selectors that are involved in the interaction
  # formula
  
  form_sel <- find_selectors(x$terms)
  
  ## Resolve the selectors to a expression containing an additive
  ## function of the variables
  
  if(length(form_sel) > 0) {
    form_res <- map(form_sel, terms_select, info = info)
    form_res <- map(form_res, vec_2_expr)
    ## Subsitute the column names into the original interaction
    ## formula. 
    for(i in seq(along = form_res)) {
      x$terms <- replace_selectors(
        x$terms,
        form_sel[[i]],
        form_res[[i]]
      )
    }
  }
  
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
  if(!is_formula(form))
    form <- as.formula(form)
  
  ## We are going to cheat and make a small fake data set to
  ## efficiently get the full formula expansion from
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

int_name <- function(x)
  get_term_names(x, all.vars(x))

#' @importFrom rlang na_dbl
#' @rdname step_interact
#' @param x A `step_interact` object
tidy.step_interact <- function(x, ...) {
  tibble(terms = vapply(x$objects, int_name, character(1)))
}

map_call <- function(x, f, ...) as.call(lapply(x, f, ...))
map_pairlist <- function(x, f, ...) as.pairlist(lapply(x, f, ...))


# In a formula, find the selectors (if any) and return the call(s)
find_selectors <- function (f) {
  if (is.function(f)) {
    find_selectors(body(f))
  } 
  else if (is.call(f)) {
    fname <- as.character(f[[1]])
    res <- if (fname %in% selectors) f else list()
    c(res, unlist(lapply(f[-1], find_selectors), use.names = FALSE))
  } 
  else if (is.name(f) || is.atomic(f)) {
    list()
  }  
  else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(f),
         call. = FALSE)
  }
}

replace_selectors <- function(x, elem, value) {
  if (is.atomic(x) || is.name(x)) {
    x
  }  else if (is.call(x)) {
    if (identical(x, elem)) {
      value
    } else {
      map_call(x, replace_selectors, elem, value)
    }
  } else if (is.pairlist(x)) {
    map_pairlist(x, replace_selectors, elem, value)
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

plus_call <- function(x, y) call("+", x, y)

#' @importFrom rlang syms !!
#' @importFrom purrr reduce
vec_2_expr <- function(x) {
  x <- rlang::syms(x)
  res <- purrr::reduce(x, plus_call)
  expr((!!res))
}
