#' Extract patterns from nominal data
#'
#' `step_dummy_extract()` creates a *specification* of a recipe step that will
#' convert nominal data (e.g. characters or factors) into one or more integer
#' model terms for the extracted levels.
#'
#' @inheritParams step_center
#' @inheritParams step_other
#' @inheritParams step_dummy
#' @param sep Character string containing a regular expression to use for
#'   splitting. [strsplit()] is used to perform the split. `sep` takes priority
#'   if `pattern` is also specified.
#' @param pattern Character string containing a regular expression used for
#'   extraction. [gregexpr()] and [regmatches()] are used to perform pattern
#'   extraction using `perl = TRUE`.
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [dummy_extract_names()]
#' @export
#' @details
#'
#' `step_dummy_extract()` will create a set of integer dummy variables from a
#' character variable by extracting individual strings by either splitting or
#' extracting then counting those to create count variables.
#'
#' Note that `threshold` works in a very specific way for this step. While it is
#' possible for one label to be present multiple times in the same row, it will
#' only be counted once when calculating the occurrences and frequencies.
#'
#' @template dummy-naming
#'
#' @details
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
#' The return value is ordered according to the frequency of `columns` entries
#' in the training data set.
#'
#' @template sparse-creation
#'
#' @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(tate_text, package = "modeldata")
#'
#' dummies <- recipe(~ artist + medium, data = tate_text) |>
#'   step_dummy_extract(artist, medium, sep = ", ") |>
#'   prep()
#'
#' dummy_data <- bake(dummies, new_data = NULL)
#'
#' dummy_data |>
#'   select(starts_with("medium")) |>
#'   names() |>
#'   head()
#'
#' # More detailed splitting
#' dummies_specific <- recipe(~medium, data = tate_text) |>
#'   step_dummy_extract(medium, sep = "(, )|( and )|( on )") |>
#'   prep()
#'
#' dummy_data_specific <- bake(dummies_specific, new_data = NULL)
#'
#' dummy_data_specific |>
#'   select(starts_with("medium")) |>
#'   names() |>
#'   head()
#'
#' tidy(dummies, number = 1)
#' tidy(dummies_specific, number = 1)
#'
#' # pattern argument can be useful to extract harder patterns
#' color_examples <- tibble(
#'   colors = c(
#'     "['red', 'blue']",
#'     "['red', 'blue', 'white']",
#'     "['blue', 'blue', 'blue']"
#'   )
#' )
#'
#' dummies_color <- recipe(~colors, data = color_examples) |>
#'   step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')") |>
#'   prep()
#'
#' dummies_data_color <- dummies_color |>
#'   bake(new_data = NULL)
#'
#' dummies_data_color
step_dummy_extract <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    sep = NULL,
    pattern = NULL,
    threshold = 0.0,
    other = "other",
    naming = dummy_extract_names,
    levels = NULL,
    sparse = "auto",
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("dummy_extract")
  ) {
    add_step(
      recipe,
      step_dummy_extract_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        sep = sep,
        pattern = pattern,
        threshold = threshold,
        other = other,
        naming = naming,
        levels = levels,
        sparse = sparse,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_dummy_extract_new <-
  function(
    terms,
    role,
    trained,
    sep,
    pattern,
    threshold,
    other,
    naming,
    levels,
    sparse,
    keep_original_cols,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "dummy_extract",
      terms = terms,
      role = role,
      trained = trained,
      sep = sep,
      pattern = pattern,
      threshold = threshold,
      other = other,
      naming = naming,
      levels = levels,
      sparse = sparse,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_dummy_extract <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))
  if (x$threshold >= 1) {
    check_number_whole(x$threshold, arg = "threshold")
  } else {
    check_number_decimal(x$threshold, min = 0, arg = "threshold")
  }
  check_string(x$other, arg = "other", allow_null = TRUE)
  check_string(x$sep, arg = "sep", allow_null = TRUE)
  check_string(x$pattern, arg = "pattern", allow_null = TRUE)
  check_function(x$naming, arg = "naming", allow_empty = FALSE)
  check_sparse_arg(x$sparse)

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  if (length(col_names) > 0) {
    levels <- vector(mode = "list", length = length(col_names))
    names(levels) <- col_names
    for (col_name in col_names) {
      elements <- dummy_extract(
        training[[col_name]],
        sep = x$sep,
        pattern = x$pattern
      )

      lvls <- map(elements, unique)

      if (is.null(wts)) {
        wts_tab <- NULL
      } else {
        wts_tab <- purrr::map2(
          lvls,
          as.double(wts),
          \(.x, .y) rep(.y, length(.x))
        )
        wts_tab <- unlist(wts_tab)
      }

      lvls <- unlist(lvls)

      lvls <- sort(weighted_table(lvls, wts = wts_tab), decreasing = TRUE)

      if (x$threshold < 1) {
        if (is.null(wts)) {
          wts_total <- length(elements)
        } else {
          wts_total <- sum(as.double(wts))
        }
        lvls <- lvls[(lvls / length(elements)) >= x$threshold]
      } else {
        lvls <- lvls[lvls >= x$threshold]
      }

      lvls <- names(lvls)

      levels[[col_name]] <- lvls
    }
  } else {
    levels <- NULL
  }

  step_dummy_extract_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    sep = x$sep,
    pattern = x$pattern,
    threshold = x$threshold,
    other = x$other,
    naming = x$naming,
    levels = levels,
    sparse = x$sparse,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_dummy_extract <- function(object, new_data, ...) {
  col_names <- names(object$levels)
  check_new_data(col_names, object, new_data)

  # If no terms were selected
  if (length(object$levels) == 0) {
    return(new_data)
  }

  for (col_name in col_names) {
    elements <- dummy_extract(
      new_data[[col_name]],
      sep = object$sep,
      pattern = object$pattern
    )

    indicators <- list_to_dummies(
      elements,
      sort(object$levels[[col_name]]),
      object$other,
      sparse_is_yes(object$sparse)
    )

    if (sparse_is_yes(object$sparse)) {
      indicators <- purrr::map(indicators, sparsevctrs::as_sparse_integer)
    } else {
      indicators <- purrr::map(indicators, vec_cast, integer())
    }

    indicators <- tibble::new_tibble(indicators)

    ## use backticks for nonstandard factor levels here
    used_lvl <- gsub(paste0("^", col_name), "", colnames(indicators))
    colnames(indicators) <- object$naming(col_name, used_lvl)

    indicators <- check_name(indicators, new_data, object, names(indicators))

    new_data <- vec_cbind(new_data, indicators, .name_repair = "minimal")
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

dummy_extract <- function(x, sep = NULL, pattern = NULL, call = caller_env()) {
  x <- as.character(x)
  if (!is.null(sep)) {
    return(strsplit(x, sep))
  }
  if (!is.null(pattern)) {
    matches <- gregexpr(pattern = pattern, text = x, perl = TRUE)
    return(regmatches(x, m = matches))
  }
  cli::cli_abort("{.arg sep} or {.arg pattern} must be specified.", call = call)
}

list_to_dummies <- function(x, dict, other = "other", sparse = FALSE) {
  i <- rep(seq_along(x), lengths(x))
  j <- match(unlist(x), dict)

  dict <- c(dict, other)
  j[is.na(j)] <- length(dict)

  out <- Matrix::sparseMatrix(
    i = i,
    j = j,
    dims = c(length(x), length(dict)),
    x = 1
  )

  out@Dimnames[[2]] <- dict

  if (sparse) {
    out <- sparsevctrs::coerce_to_sparse_tibble(out)
  } else {
    out <- as.matrix(out)
    out <- tibble::as_tibble(out)
  }

  out
}

#' @export
print.step_dummy_extract <-
  function(x, width = max(20, options()$width - 20), ...) {
    title <- "Extract patterns from "
    print_step(
      names(x$levels),
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_dummy_extract <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$levels) > 0) {
      res <- purrr::map(x$levels, \(.x) tibble(columns = .x))
      res <- purrr::list_rbind(res, names_to = "terms")
    } else {
      res <- tibble(terms = character(), columns = character())
    }
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}

#' @export
.recipes_estimate_sparsity.step_dummy_extract <- function(x, data, ...) {
  get_levels <- function(col) {
    elements <- dummy_extract(
      col[seq(1, min(10, length(col)))],
      sep = x$sep,
      pattern = x$pattern
    )

    lvls <- map(elements, unique)
    lvls <- unlist(lvls)
    length(lvls)
  }

  n_levels <- lapply(data, get_levels)

  lapply(n_levels, function(n_lvl) {
    c(
      n_cols = n_lvl,
      sparsity = 1 - 1 / n_lvl
    )
  })
}
