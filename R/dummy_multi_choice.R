#'  Multi Choice Dummy Variables Creation
#'
#' `step_dummy_multi_choice()` creates a *specification* of a recipe
#'  step that will convert multiple nominal data (e.g. character or factors)
#'  into one or more numeric binary model terms for the levels of
#'  the original data.
#'
#' @inheritParams step_center
#' @inheritParams step_dummy
#' @param ... One or more selector functions to choose which variables will be
#'  used to compute the components. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the binary dummy variable columns created by the original
#'  variables will be used as predictors in a model.
#' @param threshold A numeric value between 0 and 1. If less than one,
#'  factor levels whose rate of occurrence in the training set are below
#'  `threshold` will be "othered". See Details below.
#'   If `threshold = 0` then no thresholding is done.
#' @param levels A list with the information needed to
#'  create dummy variables for each variable contained in
#'  `terms`. This is `NULL` until the step is trained by
#'  [prep.recipe()].
#' @param input A character vector containing the names of the columns used.
#'  This is `NULL` until the step is trained by [prep.recipe()].
#' @param other A single character value for the "other" category.
#' @param prefix A character string that will be the prefix to the resulting
#'  new variables. See notes below.
#' @param keep_original_cols A logical to keep the original variables in the
#'  output. Defaults to `FALSE`.
#' @template step-return
#' @keywords datagen
#' @concept preprocessing
#' @concept dummy_multi_choice
#' @concept projection_methods
#' @export
#' @details
#' `step_dummy_multi_choice()` will create a set of binary dummy
#'  variables from a selection of factor variables. For data such as:
#'
#' | lang_1   | lang_2  | lang_3 |
#' | -------- | ------- | ------ |
#' | English  | Italian | NA     |
#' | Spanish  | NA      | French |
#' | Armenian | English | French |
#' | NA       | NA      | NA     |
#'
#'  the results will be:
#'
#' | Armenian | English | French | Italian | Spanish |
#' | -------- | ------- | ------ | ------- | ------- |
#' | 0        | 1       | 0      | 1       | 0       |
#' | 0        | 0       | 1      | 0       | 1       |
#' | 1        | 1       | 1      | 0       | 0       |
#' | 0        | 0       | 0      | 0       | 0       |
#'
#'  This recipe step allows for flexible naming of the resulting
#'  variables. For an unordered factor named `x`, with levels `"a"`
#'  and `"b"`, the default naming convention would be to create a
#'  new variable called `x_b`. The naming format can be changed using
#'  the `naming` argument; the function [dummy_names()] is the
#'  default. This function will also change the names of ordinal
#'  dummy variables. Instead of values such as "`.L`", "`.Q`", or
#'  "`^4`", ordinal dummy variables are given simple integer
#'  suffixes such as "`_1`", "`_2`", etc.
#'
#'  The overall proportion (or total counts) of the categories are
#'  computed. The "other" category is used in place of any categorical levels
#'  whose individual proportion (or frequency) in the training set is less than
#'  `threshold`.
#'
#' @examples
#' library(tibble)
#' languages <- tribble(
#'   ~lang_1,    ~lang_2,   ~lang_3,
#'   "English",  "Italian", NA,
#'   "Spanish",  NA,        "French",
#'   "Armenian", "English", "French",
#'   NA,         NA,        NA
#' )
#'
#' dummy_multi_choice_rec <- recipe(~ ., data = languages) %>%
#'   step_dummy_multi_choice(starts_with("lang")) %>%
#'   prep()
#'
#' bake(dummy_multi_choice_rec, new_data = NULL)
#' tidy(dummy_multi_choice_rec, number = 1)
#'
#' dummy_multi_choice_rec2 <- recipe(~ ., data = languages) %>%
#'   step_dummy_multi_choice(starts_with("lang"), prefix = "lang",
#'                          threshold = 0.2) %>%
#'   prep()
#'
#' bake(dummy_multi_choice_rec2, new_data = NULL)
#' tidy(dummy_multi_choice_rec2, number = 1)
step_dummy_multi_choice <- function(recipe,
                     ...,
                     role = "predictor",
                     trained = FALSE,
                     threshold = 0,
                     levels = NULL,
                     input = NULL,
                     other = "other",
                     naming = dummy_names,
                     prefix = NULL,
                     keep_original_cols = FALSE,
                     skip = FALSE,
                     id = rand_id("dummy_multi_choice")) {

  if (!is_tune(threshold) & !is_varying(threshold)) {
    if (threshold < 0) {
      rlang::abort("`threshold` should be non-negative.")
    }
    if (threshold > 1) {
      rlang::abort("`threshold` should be less then or equal to 1.")
    }
  }

  add_step(
    recipe,
    step_dummy_multi_choice_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      threshold = threshold,
      levels = levels,
      input = input,
      other = other,
      naming = naming,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_dummy_multi_choice_new <-
  function(terms, role, trained, threshold, levels, input, other, naming,
           prefix,  keep_original_cols, skip, id) {
    step(
      subclass = "dummy_multi_choice",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      levels = levels,
      input = input,
      other = other,
      naming = naming,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_dummy_multi_choice <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  multi_dummy_check_type(training[, col_names])

  levels <- purrr::map(training[, col_names], as.character)
  levels <- unlist(levels)
  levels <- levels[!is.na(levels)]
  levels <- keep_levels(levels, x$threshold, other = x$other)

  step_dummy_multi_choice_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    levels = levels,
    input = col_names,
    other = x$other,
    naming = x$naming,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

multi_dummy_check_type <- function(dat) {
  is_good <- function(x) {
    is.factor(x) | is.character(x) | all(is.na(x))
  }

  all_good <- vapply(dat, is_good, logical(1))
  label <- "factor, character, or NA"
  if (!all(all_good))
    rlang::abort(
      paste0(
        "All columns selected for the step",
        " should be ",
        label)
    )
  invisible(all_good)
}

#' @export
bake.step_dummy_multi_choice <- function(object, new_data, ...) {

  col_names <- object$input

  indicators <- multi_dummy(new_data[, col_names], object$levels)

  prefix <- object$prefix %||% col_names[1]

  used_lvl <- gsub(paste0("^", prefix), "", colnames(indicators))
  colnames(indicators) <- object$naming(prefix, used_lvl)

  new_data <- bind_cols(new_data, as_tibble(indicators))
  keep_original_cols <- get_keep_original_cols(object)

  if (!keep_original_cols) {
    new_data <- new_data[, !(colnames(new_data) %in% col_names), drop = FALSE]
  }

  as_tibble(new_data)
}

multi_dummy <- function(x, y) {
  row_id <- rep(seq_len(nrow(x)), times = ncol(x))
  values <- unlist(purrr::map(x, as.character), use.names = FALSE)

  if (y$collapse) {
    values[(!values %in% y$keep) & !is.na(values)] <- y$other
  }

  row_id <- row_id[!is.na(values)]
  values <- values[!is.na(values)]

  values <- factor(values)

  res <- Matrix::sparseMatrix(
    i = row_id,
    j = as.numeric(values),
    dims = c(nrow(x), length(levels(values)))
  )

  colnames(res) <- levels(values)

  res <- as.matrix(res)
  if (ncol(res) > 0) {
    res <- apply(res, 2, as.integer, simplify = FALSE)
  }
  as_tibble(res)
}

print.step_dummy_multi_choice <-
  function(x, width = max(20, options()$width - 20), ...) {
    if (x$trained) {
      cat("Multi-choice Dummy variables from ")
      cat(format_ch_vec(x$input, width = width))
    } else {
      cat("Multi-choice Dummy variables from ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_dummy` object.
#' @export
tidy.step_dummy_multi_choice <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$input) > 0) {
      if (x$levels$collapse) {
        columns <- c(x$levels$keep, x$levels$other)
      } else {
        columns <- x$levels$keep
      }

      res <- tibble(terms = x$input[1],
                    columns = columns)
    } else {
      res <- tibble(terms = rlang::na_chr, columns = rlang::na_chr)
    }
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#' @rdname tunable.step
#' @export
tunable.step_dummy_multi_choice <- function(x, ...) {
  tibble::tibble(
    name = c("threshold"),
    call_info = list(
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_dummy_multi_choice",
    component_id = x$id
  )
}
