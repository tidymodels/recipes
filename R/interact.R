#' Create interaction variables
#'
#' `step_interact()` creates a *specification* of a recipe step that will create
#' new columns that are interaction terms between two or more variables.
#'
#' @inheritParams step_classdist
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param terms A traditional R formula that contains interaction terms. This
#'   can include `.` and selectors. See [selections()] for more details, and
#'   consider using [tidyselect::starts_with()] when dummy variables have been
#'   created.
#' @param objects A list of `terms` objects for each individual interaction.
#' @param sep A character value used to delineate variables in an interaction
#'   (e.g. `var1_x_var2` instead of the more traditional `var1:var2`).
#' @template step-return
#' @export
#' @details
#'
#' `step_interact()` can create interactions between variables. It is primarily
#' intended for **numeric data**; categorical variables should probably be
#' converted to dummy variables using [step_dummy()] prior to being used for
#' interactions.
#'
#' Unlike other step functions, the `terms` argument should be a traditional R
#' model formula but should contain no inline functions (e.g. `log`). For
#' example, for predictors `A`, `B`, and `C`, a formula such as `~A:B:C` can be
#' used to make a three way interaction between the variables. If the formula
#' contains terms other than interactions (e.g. `(A+B+C)^3`) only the
#' interaction terms are retained for the design matrix.
#'
#' The separator between the variables defaults to "`_x_`" so that the three way
#' interaction shown previously would generate a column named `A_x_B_x_C`. This
#' can be changed using the `sep` argument.
#'
#' When dummy variables are created and are used in interactions, selectors can
#' help specify the interactions succinctly. For example, suppose a factor
#' column `X` gets converted to dummy variables `x_2`, `x_3`, ..., `x_6` using
#' [step_dummy()]. If you wanted an interaction with numeric column `z`, you
#' could create a set of specific interaction effects (e.g. `x_2:z + x_3:z` and
#' so on) or you could use `starts_with("x_"):z`. When [prep()] evaluates this
#' step, `starts_with("x_")` resolves to `(x_2 + x_3 + x_4 + x_5 + x_6)` so that
#' the formula is now `(x_2 + x_3 + x_4 + x_5 + x_6):z` and all two-way
#' interactions are created.
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
#' data(penguins, package = "modeldata")
#' penguins <- penguins |> na.omit()
#'
#' rec <- recipe(flipper_length_mm ~ ., data = penguins)
#'
#' int_mod_1 <- rec |>
#'   step_interact(terms = ~ bill_depth_mm:bill_length_mm)
#'
#' # specify all dummy variables succinctly with `starts_with()`
#' int_mod_2 <- rec |>
#'   step_dummy(sex, species, island) |>
#'   step_interact(terms = ~ body_mass_g:starts_with("species"))
#'
#' int_mod_1 <- prep(int_mod_1, training = penguins)
#' int_mod_2 <- prep(int_mod_2, training = penguins)
#'
#' dat_1 <- bake(int_mod_1, penguins)
#' dat_2 <- bake(int_mod_2, penguins)
#'
#' names(dat_1)
#' names(dat_2)
#'
#' tidy(int_mod_1, number = 1)
#' tidy(int_mod_2, number = 2)
step_interact <-
  function(
    recipe,
    terms,
    role = "predictor",
    trained = FALSE,
    objects = NULL,
    sep = "_x_",
    keep_original_cols = TRUE,
    skip = FALSE,
    id = rand_id("interact")
  ) {
    add_step(
      recipe,
      step_interact_new(
        terms = enquos(terms),
        trained = trained,
        role = role,
        objects = objects,
        sep = sep,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

## Initializes a new object
step_interact_new <-
  function(terms, role, trained, objects, sep, keep_original_cols, skip, id) {
    check_string(sep, call = rlang::call2("step_interact"))
    step(
      subclass = "interact",
      terms = terms,
      role = role,
      trained = trained,
      objects = objects,
      sep = sep,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

## The idea is to save a bunch of x-factor interaction terms instead of
## one large set of collected terms.
#' @export
prep.step_interact <- function(x, training, info = NULL, ...) {
  # Empty selection
  if (identical(x$terms[[1]], quo())) {
    return(
      step_interact_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        objects = x$objects,
        sep = x$sep,
        keep_original_cols = x$keep_original_cols,
        skip = x$skip,
        id = x$id
      )
    )
  }

  # make backwards compatible with 1.0.6 (#1138)
  if (!is_formula(x)) {
    tmp_terms <- tryCatch(
      rlang::eval_tidy(x$terms[[1]]),
      error = function(cnd) {
        cli::cli_abort(
          "{.arg terms} must be supplied as a formula.",
          call = NULL
        )
      }
    )
    if (!is_formula(tmp_terms)) {
      # Have not been able to reach
      cli::cli_abort(
        "{.arg terms} must be a formula, not {.obj_type_friendly {term}}.",
        .internal = TRUE
      )
    }

    environment(tmp_terms) <- environment(x$terms[[1]])
    x$terms <- tmp_terms
  }

  # Identify any selectors that are involved in the interaction
  # formula
  form_sel <- find_selectors(x$terms)

  # Use formula environment as quosure env
  env <- rlang::f_env(x$terms)

  recipes_eval_select_expr <- function(expr) {
    # Wrap `expr` into a list-of-quos as `recipes_eval_select()` expects
    quo <- new_quosure(expr, env)
    quos <- list(quo)
    recipes_eval_select(quos, data = training, info = info)
  }

  ## Resolve the selectors to a expression containing an additive
  ## function of the variables
  if (length(form_sel) > 0) {
    form_res <- map(form_sel, recipes_eval_select_expr)
    form_res <- map(form_res, vec_2_expr)
    ## Subsitute the column names into the original interaction
    ## formula.
    for (i in seq(along.with = form_res)) {
      x$terms <- replace_selectors(
        x$terms,
        form_sel[[i]],
        form_res[[i]]
      )
    }
  }

  ## First, find the interaction terms based on the given formula
  int_terms <- get_term_names(x$terms, vnames = colnames(training))

  if (!all(is.na(int_terms))) {
    ## Check to see if any variables are non-numeric and issue a warning
    ## if that is the case
    vars <-
      unique(unlist(lapply(make_new_formula(int_terms), all.vars)))
    var_check <- info[info$variable %in% vars, ]
    if (any(vapply(var_check$type, function(x) "nominal" %in% x, logical(1)))) {
      cli::cli_warn(
        "Categorical variables used in {.fn step_interact} should probably be \\
        avoided; This can lead to differences in dummy variable values that \\
        are produced by {.help [?step_dummy](recipes::step_dummy)}. Please \\
        convert all involved variables to dummy variables first."
      )
    }

    ## For each interaction, create a new formula that has main effects
    ## and only the interaction of choice (e.g. `a+b+c+a:b:c`)
    int_forms <- make_new_formula(int_terms)

    ## Generate a standard R `terms` object from these short formulas and
    ## save to make future interactions
    int_terms <- make_small_terms(int_forms, training)
  }

  step_interact_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    objects = int_terms,
    sep = x$sep,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_interact <- function(object, new_data, ...) {
  # empty selection
  if (is.null(object$objects)) {
    return(new_data)
  }

  col_names <- unlist(
    lapply(object$objects, function(x) all.vars(rlang::f_rhs(x)))
  )
  check_new_data(col_names, object, new_data)

  # When the interaction specification failed, just move on
  if (isTRUE(all(is.na(object$object)))) {
    return(new_data)
  }

  ## `na.action` cannot be passed to `model.matrix` but we
  ## can change it globally for a bit

  old_opt <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = old_opt))

  ## Create low level model matrices then remove the non-interaction terms.
  res <- lapply(object$object, model.matrix, data = new_data)
  options(na.action = old_opt)
  on.exit(expr = NULL)

  res <-
    lapply(res, function(x) {
      x[, grepl(":", colnames(x)), drop = FALSE]
    })
  ncols <- vapply(res, ncol, c(int = 1L))
  out <- matrix(NA, nrow = nrow(new_data), ncol = sum(ncols))
  strt <- 1
  for (i in seq_along(ncols)) {
    cols <- (strt):(strt + ncols[i] - 1)
    out[, cols] <- res[[i]]
    strt <- max(cols) + 1
  }
  colnames(out) <-
    gsub(":", object$sep, unlist(lapply(res, colnames)))
  out <- as_tibble(out)
  out <- check_name(out, new_data, object, names(out))
  new_data <- vec_cbind(new_data, out, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

## This uses the highest level of interactions
x_fac_int <- function(x) {
  as.formula(
    paste0(
      "~",
      paste0(x, collapse = "+"),
      "+",
      paste0(x, collapse = ":")
    )
  )
}

make_new_formula <- function(x) {
  splitup <- strsplit(x, ":")
  lapply(splitup, x_fac_int)
}

## Given a standard model formula and some data, get the
## term expansion (without `.`s). This returns the factor
## names and would not expand dummy variables.
get_term_names <- function(form, vnames) {
  if (!rlang::is_formula(form, scoped = TRUE)) {
    form <- as.formula(form)
  }

  ## We are going to cheat and make a small fake data set to
  ## efficiently get the full formula expansion from
  ## model.matrix (devoid of factor levels) and then
  ## pick off the interactions
  dat <- matrix(1, nrow = 5, ncol = length(vnames))
  colnames(dat) <- vnames
  nms <- try(
    colnames(model.matrix(form, data = as.data.frame(dat))),
    silent = TRUE
  )
  if (inherits(nms, "try-error")) {
    # have not been able to reach
    cli::cli_warn(
      c(
        "!" = "Interaction specification failed for:",
        "*" = deparse(form),
        "i" = "No interactions will be created"
      )
    )
    return(rlang::na_chr)
  }
  nms <- nms[nms != "(Intercept)"]
  nms <- grep(":", nms, value = TRUE)
  nms
}

## For a given data set and a list of formulas, generate the
## standard R `terms` objects
make_small_terms <- function(forms, dat) {
  lapply(forms, terms, data = dat)
}

#' @export
print.step_interact <-
  function(x, width = max(20, options()$width - 27), ...) {
    title <- "Interactions with "
    if (x$trained) {
      terms <- map_chr(
        x$objects,
        function(x) utils::tail(attr(x, "term.labels"), 1)
      )
    } else {
      terms <- as_label(x$terms[[1]])
      if (terms == "<empty>") {
        terms <- ""
      } else {
        terms <- as.character(as.formula(terms))[-1]
      }
    }
    untrained_terms <- rlang::parse_quos(terms, rlang::current_env())
    print_step(terms, untrained_terms, x$trained, title, width)
    invisible(x)
  }

int_name <- function(x) {
  if (inherits(x, "terms")) {
    res <- get_term_names(x, all.vars(x))
  } else {
    res <- rlang::na_chr
  }
  res
}

#' @rdname tidy.recipe
#' @export
tidy.step_interact <- function(x, ...) {
  res <- tibble(terms = vapply(x$objects, int_name, character(1)))
  res$id <- x$id
  res
}

map_call <- function(x, f, ...) as.call(lapply(x, f, ...))
map_pairlist <- function(x, f, ...) as.pairlist(lapply(x, f, ...))

# In a formula, find the selectors (if any) and return the call(s)
find_selectors <- function(f) {
  if (is.function(f)) {
    find_selectors(body(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])
    fname <- fname[!(fname %in% c("::", "tidyselect", "dplyr", "recipes"))]

    res <- if (fname %in% intersect_selectors) f else list()
    c(res, unlist(lapply(f[-1], find_selectors), use.names = FALSE))
  } else if (is.name(f) || is.atomic(f)) {
    list()
  } else {
    # User supplied incorrect input
    # have not been able to reach
    cli::cli_abort(
      "Don't know how to handle type {.code {typeof(f)}}.",
      .internal = TRUE
    )
  }
}

replace_selectors <- function(x, elem, value) {
  if (is.atomic(x) || is.name(x)) {
    x
  } else if (is.call(x)) {
    if (identical(x, elem)) {
      value
    } else {
      map_call(x, replace_selectors, elem, value)
    }
  } else if (is.pairlist(x)) {
    map_pairlist(x, replace_selectors, elem, value)
  } else {
    # User supplied incorrect input
    # have not been able to reach
    cli::cli_abort(
      "Don't know how to handle type {.code {typeof(f)}}.",
      .internal = TRUE
    )
  }
}

intersect_selectors <- c(
  "starts_with",
  "ends_with",
  "contains",
  "matches",
  "num_range",
  "everything",
  "one_of",
  "all_of",
  "any_of",
  "c",
  "where",
  "has_role",
  "all_predictors",
  "all_numeric_predictors",
  "all_nominal_predictors",
  "all_outcomes",
  "has_type",
  "all_numeric",
  "all_nominal"
)

plus_call <- function(x, y) call("+", x, y)

vec_2_expr <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }
  x <- rlang::syms(x)
  res <- purrr::reduce(x, plus_call)
  expr((!!res))
}
