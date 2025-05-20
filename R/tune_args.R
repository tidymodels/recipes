#' @export
tune_args.recipe <- function(object, full = FALSE, ...) {
  steps <- object$steps

  if (length(steps) == 0L) {
    return(tune_tbl())
  }

  res <- list()
  for (i in seq_along(object$steps)) {
    step <- object$steps[[i]]
    res[[i]] <- tune_args(step, full = full)
  }
  res <- purrr::list_rbind(res)

  tune_tbl(
    res$name,
    res$tunable,
    res$id,
    res$source,
    res$component,
    res$component_id,
    full = full
  )
}

#' @export
tune_args.step <- function(object, full = FALSE, ...) {
  step_id <- object$id
  # Grab the step class before the subset, as that removes the class
  step_type <- class(object)[1]

  tune_param_list <- tunable(object)$name

  # Remove NULL argument steps. These are reserved
  # for deprecated args or those set at prep() time.
  object <- object[!purrr::map_lgl(object, is.null)]

  res <- character(length(object))
  names(res) <- names(object)

  for (i in seq_along(res)) {
    res[[i]] <- find_tune_id(object[[i]])
  }
  res <- ifelse(res == "", names(res), res)

  is_tunable <- unname(!is.na(res)) & res %in% tune_param_list

  tune_tbl(
    name = names(res),
    tunable = is_tunable,
    id = unname(res),
    source = "recipe",
    component = step_type,
    component_id = step_id,
    full = full
  )
}

#' @export
tune_args.check <- tune_args.step

# helpers for tune_args() methods -----------------------------------------
# they also exist in parsnip for the `tune_args()` method there

# useful for standardization and for creating a 0 row tunable tbl
# (i.e. for when there are no steps in a recipe)
tune_tbl <- function(
  name = character(),
  tunable = logical(),
  id = character(),
  source = character(),
  component = character(),
  component_id = character(),
  full = FALSE,
  call = rlang::caller_env()
) {
  complete_id <- id[!is.na(id)]
  dups <- duplicated(complete_id)
  if (any(dups)) {
    offenders <- unique(complete_id[dups])
    cli::cli_abort(
      "There are duplicate {.field id} values listed in {.fn tune}: \\
      {.val {offenders}}.",
      call = call
    )
  }

  vry_tbl <- tibble::tibble(
    name = as.character(name),
    tunable = as.logical(tunable),
    id = as.character(id),
    source = as.character(source),
    component = as.character(component),
    component_id = as.character(component_id)
  )

  if (!full) {
    vry_tbl <- vry_tbl[!is.na(vry_tbl$id), ]
  }

  vry_tbl
}

# Return the `id` arg in tune(); if not specified, then returns "" or if not
# a tunable arg then returns NA_character_
tune_id <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  } else {
    if (rlang::is_quosures(x)) {
      # Try to evaluate to catch things in the global envir.
      .x <- try(purrr::map(x, rlang::eval_tidy), silent = TRUE)
      if (inherits(.x, "try-error")) {
        x <- purrr::map(x, rlang::quo_get_expr)
      } else {
        x <- .x
      }
      if (is.null(x)) {
        return(NA_character_)
      }
    }

    # [tune()] will always return a call object
    if (is.call(x)) {
      if (identical(rlang::call_name(x), "tune")) {
        # If an id was specified:
        if (length(x) > 1) {
          return(x[[2]])
        } else {
          # no id
          return("")
        }
        return(x$id)
      } else {
        return(NA_character_)
      }
    }
  }
  NA_character_
}

find_tune_id <- function(x) {
  # STEP 1 - Early exits

  # Early exit for empty elements (like list())
  if (length(x) == 0L) {
    return(NA_character_)
  }
  if (is.list(x) && !inherits(x, "list")) {
    return(NA_character_)
  }

  # turn quosures into expressions before continuing
  if (rlang::is_quosures(x)) {
    # Try to evaluate to catch things in the global envir. If it is a dplyr
    # selector, it will fail to evaluate.
    .x <- try(purrr::map(x, rlang::eval_tidy), silent = TRUE)
    if (inherits(.x, "try-error")) {
      x <- purrr::map(x, rlang::quo_get_expr)
    } else {
      x <- .x
    }
  }

  id <- tune_id(x)
  if (!is.na(id)) {
    return(id)
  }

  if (is.atomic(x) | is.name(x) | length(x) == 1) {
    return(NA_character_)
  }

  # STEP 2 - Recursion

  # tunable_elems <- purrr::map_lgl(x, find_tune)
  tunable_elems <- vector("character", length = length(x))

  # use purrr::map_lgl
  for (i in seq_along(x)) {
    tunable_elems[i] <- find_tune_id(x[[i]])
  }

  tunable_elems <- tunable_elems[!is.na(tunable_elems)]
  if (length(tunable_elems) == 0) {
    tunable_elems <- NA_character_
  }

  if (sum(tunable_elems == "", na.rm = TRUE) > 1) {
    offenders <- paste0(deparse(x), collapse = "")
    cli::cli_abort(
      "Only one tunable value is currently allowed per argument. \\
      The current argument has: {offenders}."
    )
  }

  return(tunable_elems)
}
