#' Collapse infrequent categorical levels
#'
#' `step_other()` creates a *specification* of a recipe step that will
#' potentially pool infrequently occurring values into an `"other"` category.
#'
#' @inheritParams step_center
#' @param threshold A numeric value between 0 and 1, or an integer greater or
#'   equal to one.  If less than one, then factor levels with a rate of
#'   occurrence in the training set below `threshold` will be pooled to `other`.
#'   If greater or equal to one, then this value is treated as a frequency and
#'   factor levels that occur less than `threshold` times will be pooled to
#'   `other`.
#' @param other A single character value for the other category, default to
#'   `"other"`.
#' @param objects A list of objects that contain the information to pool
#'   infrequent levels that is determined by [prep()].
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [dummy_names()]
#' @export
#' @details
#'
#' The overall proportion (or total counts) of the categories are computed. The
#' `other` category is used in place of any categorical levels whose individual
#' proportion (or frequency) in the training set is less than `threshold`.
#'
#' If no pooling is done the data are unmodified (although character data may be
#' changed to factors based on the value of `strings_as_factors` in [prep()]).
#' Otherwise, a factor is always returned with different factor levels.
#'
#' If `threshold` is less than the largest category proportion, all levels
#' except for the most frequent are collapsed to the `other` level.
#'
#' If the retained categories include the value of `other`, an error is thrown.
#' If `other` is in the list of discarded levels, no error occurs.
#'
#' If no pooling is done, novel factor levels are converted to missing. If
#' pooling is needed, they will be placed into the other category.
#'
#' When data to be processed contains novel levels (i.e., not contained in the
#' training set), the other category is assigned.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `retained` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{retained}{character, factor levels not pulled into `"other"`}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_other"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' set.seed(19)
#' in_train <- sample(1:nrow(Sacramento), size = 800)
#'
#' sacr_tr <- Sacramento[in_train, ]
#' sacr_te <- Sacramento[-in_train, ]
#'
#' rec <- recipe(~ city + zip, data = sacr_tr)
#'
#'
#' rec <- rec |>
#'   step_other(city, zip, threshold = .1, other = "other values")
#' rec <- prep(rec, training = sacr_tr)
#'
#' collapsed <- bake(rec, sacr_te)
#' table(sacr_te$city, collapsed$city, useNA = "always")
#'
#' tidy(rec, number = 1)
#'
#' # novel levels are also "othered"
#' tahiti <- Sacramento[1, ]
#' tahiti$zip <- "a magical place"
#' bake(rec, tahiti)
#'
#' # threshold as a frequency
#' rec <- recipe(~ city + zip, data = sacr_tr)
#'
#' rec <- rec |>
#'   step_other(city, zip, threshold = 2000, other = "other values")
#' rec <- prep(rec, training = sacr_tr)
#'
#' tidy(rec, number = 1)
#' # compare it to
#' # sacr_tr |> count(city, sort = TRUE) |> top_n(4)
#' # sacr_tr |> count(zip, sort = TRUE) |> top_n(3)
step_other <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    threshold = .05,
    other = "other",
    objects = NULL,
    skip = FALSE,
    id = rand_id("other")
  ) {
    add_step(
      recipe,
      step_other_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        threshold = threshold,
        other = other,
        objects = objects,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_other_new <-
  function(
    terms,
    role,
    trained,
    threshold,
    other,
    objects,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "other",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      other = other,
      objects = objects,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_other <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  if (!is.numeric(x$threshold)) {
    cli::cli_abort(
      "{.arg threshold} should be a single numeric value
                   {.obj_type_friendly {x$threshold}}"
    )
  }

  if (x$threshold >= 1) {
    check_number_whole(x$threshold, arg = "threshold", min = 1)
  } else {
    check_number_decimal(x$threshold, arg = "threshold", min = 0)
  }

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  objects <- lapply(
    training[, col_names],
    keep_levels,
    threshold = x$threshold,
    other = x$other,
    wts = wts
  )

  step_other_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    other = x$other,
    objects = objects,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_other <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    if (!object$objects[[col_name]]$collapse) {
      next
    }
    tmp <- new_data[[col_name]]

    if (!is.character(tmp)) {
      tmp <- as.character(tmp)
    }

    tmp <- ifelse(
      !(tmp %in% object$objects[[col_name]]$keep) & !is.na(tmp),
      object$objects[[col_name]]$other,
      tmp
    )

    # assign other factor levels other here too.
    tmp <- factor(
      tmp,
      levels = c(
        object$objects[[col_name]]$keep,
        object$objects[[col_name]]$other
      )
    )

    new_data[[col_name]] <- tmp
  }

  new_data
}

#' @export
print.step_other <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Collapsing factor levels for "
    if (x$trained) {
      columns <- map_lgl(x$objects, \(.x) .x$collapse)
      columns <- names(columns)[columns]
    } else {
      columns <- names(x$objects)
    }
    print_step(
      columns,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

keep_levels <- function(
  x,
  threshold = .1,
  other = "other",
  wts = NULL,
  call = caller_env(2)
) {
  if (!is.factor(x)) {
    x <- factor(x)
  }

  xtab <- sort(weighted_table(x, wts = wts), decreasing = TRUE)

  if (threshold < 1) {
    if (is.null(wts)) {
      xtab <- xtab / sum(!is.na(x))
    } else {
      xtab <- xtab / sum(as.double(wts)[!is.na(x)])
    }
  }

  dropped <- which(xtab < threshold)
  orig <- levels(x)

  if (length(dropped) > 0) {
    keepers <- names(xtab[-dropped])
  } else {
    keepers <- orig
  }

  if (length(keepers) == 0) {
    keepers <- names(xtab)[which.max(xtab)]
  }

  if (other %in% keepers) {
    cli::cli_abort(
      "The level {other} is already a factor level that will be retained. \\
      Please choose a different value.",
      call = call
    )
  }

  list(
    keep = orig[orig %in% keepers],
    collapse = length(dropped) > 0,
    other = other
  )
}

#' @rdname tidy.recipe
#' @export
tidy.step_other <- function(x, ...) {
  if (is_trained(x)) {
    values <- purrr::map(x$objects, function(x) x$keep)
    n <- vapply(values, length, integer(1))
    values <- vctrs::list_unchop(
      values,
      ptype = character(),
      name_spec = rlang::zap()
    )
    res <- tibble(
      terms = rep(names(n), n),
      retained = values
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      retained = rep(na_chr, length(term_names))
    )
  }
  res$id <- x$id
  res
}

#' @export
tunable.step_other <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "dials", fun = "threshold", range = c(0, 0.1))
    ),
    source = "recipe",
    component = "step_other",
    component_id = x$id
  )
}
