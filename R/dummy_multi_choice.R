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
#'  This is `NULL` until the step is trained by [prep()].
#' @template step-return
#' @family dummy variable and encoding steps
#' 
#' @details
#' The overall proportion (or total counts) of the categories are computed. The 
#' `"other"` category is used in place of any categorical levels whose 
#' individual proportion (or frequency) in the training set is less than
#' `threshold`.
#' 
#' This step produces a number of columns, based on the number of categories it
#' finds. The naming of the columns is determined by the function based on the 
#' `naming` argument. The default is to return `<prefix>_<category name>`. By 
#' default `prefix` is `NULL`, which means the name of the first column  
#' selected will be used in place.
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
#' dummy_multi_choice_rec <- recipe(~., data = languages) %>%
#'   step_dummy_multi_choice(starts_with("lang")) %>%
#'   prep()
#'
#' bake(dummy_multi_choice_rec, new_data = NULL)
#' tidy(dummy_multi_choice_rec, number = 1)
#'
#' dummy_multi_choice_rec2 <- recipe(~., data = languages) %>%
#'   step_dummy_multi_choice(starts_with("lang"),
#'     prefix = "lang",
#'     threshold = 0.2
#'   ) %>%
#'   prep()
#'
#' bake(dummy_multi_choice_rec2, new_data = NULL)
#' tidy(dummy_multi_choice_rec2, number = 1)
#' 
#' @export
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

  if (!is_tune(threshold)) {
    if (threshold >= 1) {
      check_number_whole(threshold)
    } else {
      check_number_decimal(threshold, min = 0)
    }
  }

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
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_dummy_multi_choice_new <-
  function(terms, role, trained, threshold, levels, input, other, naming,
           prefix, keep_original_cols, skip, id) {
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
  check_type(training[, col_names], types = c("nominal", "logical"))

  multi_dummy_check_type(training[, col_names])

  levels <- purrr::map(training[, col_names], levels)
  levels <- vctrs::list_unchop(levels, ptype = character(), name_spec = rlang::zap())
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

multi_dummy_check_type <- function(dat, call = rlang::caller_env()) {
  is_good <- function(x) {
    is.factor(x) | is.character(x) | all(is.na(x))
  }

  all_good <- vapply(dat, is_good, logical(1))
  if (!all(all_good)) {
    offenders <- names(dat)[!all_good]
    cli::cli_abort(c(
      "x" = "All columns selected for the step should be \\
            factor, character, or NA. The following were not:",
      "*" = "{.var {offenders}}."
    ), call = call)
  }
  invisible(all_good)
}

#' @export
bake.step_dummy_multi_choice <- function(object, new_data, ...) {
  col_names <- object$input
  check_new_data(col_names, object, new_data)

  indicators <- multi_dummy(new_data[, col_names], object$levels)

  prefix <- object$prefix
  if (is.null(prefix)) {
    prefix <- if (length(col_names) >= 1) col_names[[1]] else ""
  }

  used_lvl <- gsub(paste0("^", prefix), "", colnames(indicators))
  colnames(indicators) <- object$naming(prefix, used_lvl)
  indicators <- as_tibble(indicators)

  indicators <- check_name(indicators, new_data, object, names(indicators))

  new_data <- vec_cbind(new_data, indicators)
  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

multi_dummy <- function(x, y) {
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

  res <- as.matrix(res)
  if (ncol(res) > 0) {
    res <- apply(res, 2, as.integer, simplify = FALSE)
  }

  # to preserve old behavior drop other if empty
  res <- as_tibble(res)

  if (sum(res[y$other]) == 0) {
    res <- dplyr::select(res, -y$other)
  }

  return(res)
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
