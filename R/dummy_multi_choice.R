#' Handle levels in multiple predictors together
#'
#' `step_dummy_multi_choice()` creates a *specification* of a recipe step that
#' will convert multiple nominal data (e.g. characters or factors) into one or
#' more numeric binary model terms for the levels of the original data.
#'
#' @inheritParams step_dummy
#' @inheritParams step_center
#' @inheritParams step_other
#' @inheritParams step_pca
#' @param input A character vector containing the names of the columns used.
#'   This is `NULL` until the step is trained by [prep()].
#' @template step-return
#' @family dummy variable and encoding steps
#'
#' @details
#'
#' The overall proportion (or total counts) of the categories are computed. The
#' `"other"` category is used in place of any categorical levels whose
#' individual proportion (or frequency) in the training set is less than
#' `threshold`.
#'
#' This step produces a number of columns, based on the number of categories it
#' finds. The naming of the columns is determined by the function based on the
#' `naming` argument. The default is to return `<prefix>_<category name>`. By
#' default `prefix` is `NULL`, which means the name of the first column selected
#' will be used in place.
#'
#' @template dummy-naming
#'
#' @details
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_dummy_multi_choice"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `columns` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{columns}{character, names of resulting columns}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template sparse-creation
#'
#' @template case-weights-not-supported
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
#' dummy_multi_choice_rec <- recipe(~., data = languages) |>
#'   step_dummy_multi_choice(starts_with("lang")) |>
#'   prep()
#'
#' bake(dummy_multi_choice_rec, new_data = NULL)
#' tidy(dummy_multi_choice_rec, number = 1)
#'
#' dummy_multi_choice_rec2 <- recipe(~., data = languages) |>
#'   step_dummy_multi_choice(starts_with("lang"),
#'     prefix = "lang",
#'     threshold = 0.2
#'   ) |>
#'   prep()
#'
#' bake(dummy_multi_choice_rec2, new_data = NULL)
#' tidy(dummy_multi_choice_rec2, number = 1)
#'
#' @export
step_dummy_multi_choice <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  threshold = 0,
  levels = NULL,
  input = NULL,
  other = "other",
  naming = dummy_names,
  prefix = NULL,
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("dummy_multi_choice")
) {
  add_step(
    recipe,
    step_dummy_multi_choice_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      threshold = threshold,
      levels = levels,
      input = input,
      other = other,
      naming = naming,
      prefix = prefix,
      sparse = sparse,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_dummy_multi_choice_new <-
  function(
    terms,
    role,
    trained,
    threshold,
    levels,
    input,
    other,
    naming,
    prefix,
    sparse,
    keep_original_cols,
    skip,
    id
  ) {
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
      sparse = sparse,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_dummy_multi_choice <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("nominal", "logical"))
  if (x$threshold >= 1) {
    check_number_whole(x$threshold, arg = "threshold")
  } else {
    check_number_decimal(x$threshold, min = 0, arg = "threshold")
  }
  check_string(x$other, arg = "other", allow_null = TRUE)
  check_function(x$naming, arg = "naming", allow_empty = FALSE)
  check_sparse_arg(x$sparse)

  levels <- purrr::map(training[, col_names], levels)
  levels <- vctrs::list_unchop(
    levels,
    ptype = character(),
    name_spec = rlang::zap()
  )
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
    sparse = x$sparse,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_dummy_multi_choice <- function(object, new_data, ...) {
  col_names <- object$input
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  indicators <- multi_dummy(
    new_data[, col_names],
    object$levels,
    sparse_is_yes(object$sparse)
  )

  if (sparse_is_yes(object$sparse)) {
    indicators <- purrr::map(indicators, sparsevctrs::as_sparse_integer)
  } else {
    indicators <- purrr::map(indicators, vec_cast, integer())
  }

  indicators <- tibble::new_tibble(indicators)

  prefix <- object$prefix
  if (is.null(prefix)) {
    prefix <- if (length(col_names) >= 1) col_names[[1]] else ""
  }

  used_lvl <- gsub(paste0("^", prefix), "", colnames(indicators))
  colnames(indicators) <- object$naming(prefix, used_lvl)
  indicators <- as_tibble(indicators)

  indicators <- check_name(indicators, new_data, object, names(indicators))

  new_data <- vec_cbind(new_data, indicators, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

multi_dummy <- function(x, y, sparse = FALSE) {
  row_id <- rep(seq_len(nrow(x)), times = ncol(x))
  values <- vctrs::list_unchop(
    purrr::map(x, as.character),
    ptype = character(),
    name_spec = rlang::zap()
  )

  if (y$collapse) {
    values[(!values %in% y$keep) & !is.na(values)] <- y$other
  }

  row_id <- row_id[!is.na(values)]
  values <- values[!is.na(values)]

  original_levels <- c(y$keep, y$other)

  values <- factor(values, levels = original_levels)

  res <- Matrix::sparseMatrix(
    i = row_id,
    j = as.numeric(values),
    dims = c(nrow(x), length(original_levels))
  )

  colnames(res) <- levels(values)

  if (sparse) {
    res <- sparsevctrs::coerce_to_sparse_tibble(res)
  } else {
    res <- as.matrix(res)
    res <- as_tibble(res)
  }

  if (sum(res[y$other]) == 0) {
    res <- dplyr::select(res, -y$other)
  }

  res
}

#' @export
print.step_dummy_multi_choice <-
  function(x, width = max(20, options()$width - 20), ...) {
    title <- "Multi-choice dummy variables from "
    print_step(x$input, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_dummy_multi_choice <- function(x, ...) {
  if (is_trained(x)) {
    if (x$levels$collapse) {
      columns <- c(x$levels$keep, x$levels$other)
    } else {
      columns <- x$levels$keep
    }

    if (length(x$input) >= 1) {
      terms <- x$input[[1]]
    } else {
      terms <- character()
    }

    res <- tibble(terms = terms, columns = columns)
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}

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

#' @export
.recipes_estimate_sparsity.step_dummy_multi_choice <- function(x, data, ...) {
  get_levels <- function(x) {
    if (is.factor(x)) {
      return(levels(x))
    } else {
      return(unique(x))
    }
  }

  n_levels <- purrr::map(data, get_levels)
  n_levels <- unlist(n_levels)
  n_levels <- unique(n_levels)
  n_levels <- na.omit(n_levels)
  n_levels <- length(n_levels)

  lapply(n_levels, function(n_lvl) {
    c(
      n_cols = n_lvl,
      sparsity = 1 - 1 / n_lvl
    )
  })
}
