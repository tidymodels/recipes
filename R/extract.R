#' Extract patterns from nominal data
#'
#' `step_dummy_extract()` creates a *specification* of a recipe
#'  step that will convert nominal data (e.g. character or factors)
#'  into one or more integer model terms for the extracted levels.
#'
#' @inheritParams step_center
#' @inheritParams step_other
#' @inheritParams step_dummy
#' @param sep Character vector containing a regular expression to use
#'   for splitting. [strsplit()] is used to perform the split. `sep` takes
#'   priority if `pattern` is also specified.
#' @param pattern Character vector containing a regular expression used
#'   for extraction. [gregexpr()] and [regmatches()] are used to perform
#'   pattern extraction using `perl = TRUE`.
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [dummy_extract_names()]
#' @export
#' @details `step_dummy_extract()` will create a set of integer dummy
#'  variables from a character variable by extract individual strings
#'  by either splitting or extracting then counting those to create
#'  count variables.
#'
#'  Note that `threshold` works in a very specific way for this step.
#'  While it is possible for one label to be present multiple times in
#'  the same row, it will only be counted once when calculating the
#'  occurrences and frequencies.
#'
#' @template dummy-naming
#'
#' @details
#'
#'  # Tidying
#'
#'  When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#'  `terms` (the selectors or original variables selected) and `columns`
#'  (the list of corresponding columns) is returned. The `columns` is
#'  is ordered according the frequency in the training data set.
#'
#' @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(tate_text, package = "modeldata")
#'
#' dummies <- recipe(~ artist + medium, data = tate_text) %>%
#'   step_dummy_extract(artist, medium, sep = ", ") %>%
#'   prep()
#'
#' dummy_data <- bake(dummies, new_data = NULL)
#'
#' dummy_data %>%
#'   select(starts_with("medium")) %>%
#'   names()
#'
#' # More detailed splitting
#' dummies_specific <- recipe(~medium, data = tate_text) %>%
#'   step_dummy_extract(medium, sep = "(, )|( and )|( on )") %>%
#'   prep()
#'
#' dummy_data_specific <- bake(dummies_specific, new_data = NULL)
#'
#' dummy_data_specific %>%
#'   select(starts_with("medium")) %>%
#'   names()
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
#' dummies_color <- recipe(~colors, data = color_examples) %>%
#'   step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')") %>%
#'   prep()
#'
#' dommies_data_color <- dummies_color %>%
#'   bake(new_data = NULL)
#'
#' dommies_data_color
step_dummy_extract <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           sep = NULL,
           pattern = NULL,
           threshold = 0.0,
           other = "other",
           naming = dummy_extract_names,
           levels = NULL,
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("dummy_extract")) {
    if (!is_tune(threshold) & !is_varying(threshold)) {
      if (threshold < 0) {
        rlang::abort("`threshold` should not be negative.")
      }
      if (threshold >= 1 && !is_integerish(threshold)) {
        rlang::abort("If `threshold` is greater than one it should be an integer.")
      }
    }

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
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_dummy_extract_new <-
  function(terms, role, trained, sep, pattern, threshold, other, naming, levels,
           keep_original_cols, skip, id, case_weights) {
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
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_dummy_extract <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  if (length(col_names) > 0) {
    col_names <- check_factor_vars(training, col_names, "step_dummy_extract")

    levels <- vector(mode = "list", length = length(col_names))
    names(levels) <- col_names
    for (col_name in col_names) {
      elements <- dummy_extract(
        training[[col_name]],
        sep = x$sep, pattern = x$pattern
      )

      lvls <- map(elements, unique)
      wts_tab <- purrr::map2(lvls, as.double(wts), ~rep(.y, length(.x)))
      lvls <- unlist(lvls)
      wts_tab <- unlist(wts_tab)

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
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_dummy_extract <- function(object, new_data, ...) {
  check_new_data(names(object$levels), object, new_data)

  # If no terms were selected
  if (length(object$levels) == 0) {
    return(new_data)
  }

  col_names <- names(object$levels)
  keep_original_cols <- get_keep_original_cols(object)

  for (i in seq_along(object$levels)) {
    orig_var <- names(object$levels)[i]

    elements <- dummy_extract(
      getElement(new_data, orig_var),
      sep = object$sep, pattern = object$pattern
    )

    indicators <- list_to_dummies(elements, sort(object$levels[[i]]), object$other)
    indicators <- purrr::map_dfc(indicators, vec_cast, integer())

    ## use backticks for nonstandard factor levels here
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- object$naming(col_names[i], used_lvl)

    new_data <- bind_cols(new_data, indicators)

    if (!keep_original_cols) {
      new_data[, col_names[i]] <- NULL
    }
  }
  new_data
}

dummy_extract <- function(x, sep = NULL, pattern = NULL) {
  x <- as.character(x)
  if (!is.null(sep)) {
    return(strsplit(x, sep))
  }
  if (!is.null(pattern)) {
    matches <- gregexpr(pattern = pattern, text = x, perl = TRUE)
    return(regmatches(x, m = matches))
  }
  rlang::abort("`sep` or `pattern` must be specified.")
}

list_to_dummies <- function(x, dict, other = "other") {
  i <- rep(seq_along(x), lengths(x))
  j <- match(unlist(x), dict)

  dict <- c(dict, other)
  j[is.na(j)] <- length(dict)

  out <- Matrix::sparseMatrix(
    i = i, j = j,
    dims = c(length(x), length(dict)),
    x = 1
  )

  out@Dimnames[[2]] <- dict
  out <- as.matrix(out)
  tibble::as_tibble(out)
}

print.step_dummy_extract <-
  function(x, width = max(20, options()$width - 20), ...) {
    title <- "Extract patterns from "
    print_step(names(x$levels), x$terms, x$trained, title, width,
               case_weights = x$case_weights)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_dummy_extract <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$levels) > 0) {
      res <- purrr::map_dfr(x$levels, ~ list(columns = .x), FALSE, .id = "terms")
    } else {
      res <- tibble(terms = character(), columns = character())
    }
  } else {
    res <- tibble(terms = sel2char(x$terms), columns = rlang::na_chr)
  }
  res$id <- x$id
  res
}
