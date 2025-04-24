#' Natural spline basis functions
#'
#' `step_ns()` creates a *specification* of a recipe step that will create new
#' columns that are basis expansions of variables using natural splines.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param deg_free The degrees of freedom for the natural spline. As the degrees
#'   of freedom for a natural spline increase, more flexible and complex curves
#'   can be generated. When a single degree of freedom is used, the result is a
#'   rescaled version of the original data.
#' @param objects A list of [splines::ns()] objects created once the step has
#'   been trained.
#' @param options A list of options for [splines::ns()] which should not include
#'   `x` or `df`.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details
#'
#' `step_ns()` can create new features from a single variable that enable
#' fitting routines to model this variable in a nonlinear manner. The extent of
#' the possible nonlinearity is determined by the `df` or `knots` arguments of
#' [splines::ns()]. The original variables are removed from the data and new
#' columns are added. The naming convention for the new variables is
#' `varname_ns_1` and so on.
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
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_ns"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' with_splines <- rec |>
#'   step_ns(carbon, hydrogen)
#' with_splines <- prep(with_splines, training = biomass_tr)
#'
#' expanded <- bake(with_splines, biomass_te)
#' expanded
step_ns <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    objects = NULL,
    deg_free = 2,
    options = list(),
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("ns")
  ) {
    add_step(
      recipe,
      step_ns_new(
        terms = enquos(...),
        trained = trained,
        deg_free = deg_free,
        role = role,
        objects = objects,
        options = options,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_ns_new <-
  function(
    terms,
    role,
    trained,
    deg_free,
    objects,
    options,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "ns",
      terms = terms,
      role = role,
      trained = trained,
      deg_free = deg_free,
      objects = objects,
      options = options,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

ns_statistics <- function(x, args) {
  # Only do the parameter computations from splines::bs() / splines::ns(), don't evaluate at x.
  degree <- 1L
  intercept <- as.logical(args$intercept %||% FALSE)
  # This behaves differently from splines::ns() if length(x) is 1
  boundary <- sort(args$Boundary.knots) %||% range(x)

  # This behaves differently from splines::bs() and splines::ns() if num_knots < 0L
  # the original implementations issue a warning.
  if (
    !is.null(args$df) &&
      is.null(args$knots) &&
      args$df - degree - intercept >= 1L
  ) {
    num_knots <- args$df - degree - intercept
    ok <- !is.na(x) & x >= boundary[1L] & x <= boundary[2L]
    knots <- unname(quantile(x[ok], seq_len(num_knots) / (num_knots + 1L)))
  } else {
    if (is.null(args$knots)) {
      knots <- numeric()
    } else {
      knots <- args$knots
    }
  }

  # Only construct the data necessary for splines_predict
  out <- matrix(NA, ncol = degree + length(knots) + intercept, nrow = 1L)
  class(out) <- c("ns", "basis", "matrix")
  attr(out, "degree") <- 3L
  attr(out, "knots") <- knots
  attr(out, "Boundary.knots") <- boundary
  attr(out, "intercept") <- intercept
  out
}

ns_predict <- function(object, x) {
  xu <- unique(x)
  ru <- predict(object, xu)
  res <- ru[match(x, xu), ]
  copy_attrs <- c("class", "knots", "Boundary.knots", "intercept")
  attributes(res)[copy_attrs] <- attributes(ru)[copy_attrs]
  res
}

#' @export
prep.step_ns <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer", "datetime"))
  check_options(x$options, exclude = c("x", "df"))

  opt <- x$options
  opt$df <- x$deg_free
  obj <- lapply(training[, col_names], ns_statistics, opt)
  for (i in seq(along.with = col_names)) {
    attr(obj[[i]], "var") <- col_names[i]
  }
  step_ns_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    deg_free = x$deg_free,
    objects = obj,
    options = x$options,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_ns <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_values <- bs_predict(object$objects[[col_name]], new_data[[col_name]])

    new_names <- paste(col_name, "ns", names0(ncol(new_values), ""), sep = "_")
    colnames(new_values) <- new_names

    new_values <- check_name(new_values, new_data, object, new_names)
    new_data <- vctrs::vec_cbind(new_data, new_values, .name_repair = "minimal")
  }

  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_ns <-
  function(x, width = max(20, options()$width - 28), ...) {
    title <- "Natural splines on "
    print_step(names(x$objects), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_ns <- function(x, ...) {
  if (is_trained(x)) {
    terms <- names(x$objects)
  } else {
    terms <- sel2char(x$terms)
  }
  tibble(terms = terms, id = x$id)
}

#' @export
tunable.step_ns <- function(x, ...) {
  tibble::tibble(
    name = c("deg_free"),
    call_info = list(
      list(pkg = "dials", fun = "spline_degree", range = c(1L, 15L))
    ),
    source = "recipe",
    component = "step_ns",
    component_id = x$id
  )
}
