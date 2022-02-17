#' Collapse Some Categorical Levels
#'
#' `step_other` creates a *specification* of a recipe
#'  step that will potentially pool infrequently occurring values
#'  into an "other" category.
#'
#' @inheritParams step_center
#' @param threshold A numeric value between 0 and 1, or an integer greater or
#'  equal to one.  If less than one, then factor levels with a rate of
#'  occurrence in the training set below `threshold` will be pooled to `other`.
#'  If greater or equal to one, then this value is treated as a frequency
#'  and factor levels that occur less than `threshold` times will be pooled
#'  to `other`.
#' @param other A single character value for the "other" category.
#' @param objects A list of objects that contain the information
#'  to pool infrequent levels that is determined by
#'  [prep()].
#' @template step-return
#' @family dummy variable and encoding steps
#' @seealso [dummy_names()]
#' @export
#' @details The overall proportion (or total counts) of the categories are
#'  computed. The "other" category is used in place of any categorical levels
#'  whose individual proportion (or frequency) in the training set is less than
#'  `threshold`.
#'
#' If no pooling is done the data are unmodified (although character data may
#'   be changed to factors based on the value of `strings_as_factors` in
#'   [prep()]). Otherwise, a factor is always returned with
#'   different factor levels.
#'
#' If `threshold` is less than the largest category proportion, all levels
#'   except for the most frequent are collapsed to the `other` level.
#'
#' If the retained categories include the value of `other`, an error is
#'   thrown. If `other` is in the list of discarded levels, no error
#'   occurs.
#'
#' If no pooling is done, novel factor levels are converted to missing. If
#'  pooling is needed, they will be placed into the other category.
#'
#' When data to be processed contains novel levels (i.e., not
#' contained in the training set), the other category is assigned.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the columns that will be affected) and `retained` (the factor
#' levels that were not pulled into "other") is returned.
#'
#' @examples
#' library(modeldata)
#' data(okc)
#'
#' set.seed(19)
#' in_train <- sample(1:nrow(okc), size = 30000)
#'
#' okc_tr <- okc[ in_train,]
#' okc_te <- okc[-in_train,]
#'
#' rec <- recipe(~ diet + location, data = okc_tr)
#'
#'
#' rec <- rec %>%
#'   step_other(diet, location, threshold = .1, other = "other values")
#' rec <- prep(rec, training = okc_tr)
#'
#' collapsed <- bake(rec, okc_te)
#' table(okc_te$diet, collapsed$diet, useNA = "always")
#'
#' tidy(rec, number = 1)
#'
#' # novel levels are also "othered"
#' tahiti <- okc[1,]
#' tahiti$location <- "a magical place"
#' bake(rec, tahiti)
#'
#' # threshold as a frequency
#' rec <- recipe(~ diet + location, data = okc_tr)
#'
#' rec <- rec %>%
#'   step_other(diet, location, threshold = 2000, other = "other values")
#' rec <- prep(rec, training = okc_tr)
#'
#' tidy(rec, number = 1)
#' # compare it to
#' # okc_tr %>% count(diet, sort = TRUE) %>% top_n(4)
#' # okc_tr %>% count(location, sort = TRUE) %>% top_n(3)

step_other <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           threshold = .05,
           other = "other",
           objects = NULL,
           skip = FALSE,
           id = rand_id("other")) {
    if (!is_tune(threshold) & !is_varying(threshold)) {
      if (threshold < 0) {
        rlang::abort("`threshold` should be non-negative.")
      }
      if (threshold >= 1 && !is_integerish(threshold)) {
        rlang::abort("If `threshold` is greater than one it should be an integer.")
      }
    }
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
        id = id
      )
    )
  }

step_other_new <-
  function(terms, role, trained, threshold, other, objects, skip, id) {
    step(
      subclass = "other",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      other = other,
      objects = objects,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_other <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  objects <- lapply(training[, col_names],
                    keep_levels,
                    threshold = x$threshold,
                    other = x$other)

  step_other_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    other = x$other,
    objects = objects,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_other <- function(object, new_data, ...) {
  for (i in names(object$objects)) {
    if (object$objects[[i]]$collapse) {
      tmp <- if (!is.character(new_data[, i]))
        as.character(getElement(new_data, i))
      else
        getElement(new_data, i)

      tmp <- ifelse(
        !(tmp %in% object$objects[[i]]$keep) & !is.na(tmp),
        object$objects[[i]]$other,
        tmp
      )

      # assign other factor levels other here too.
      tmp <- factor(tmp,
                    levels = c(object$objects[[i]]$keep,
                               object$objects[[i]]$other))

      new_data[, i] <- tmp
    }
  }
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_other <-
  function(x, width = max(20, options()$width - 30), ...) {

  title <- "Collapsing factor levels for "
  if (x$trained) {
    columns <- map_lgl(x$objects, ~ .x$collapse)
    columns <- names(columns)[columns]
  } else {
    columns <- names(x$objects)
  }
  print_step(columns, x$terms, x$trained, title, width)
  invisible(x)
}

keep_levels <- function(x, threshold = .1, other = "other") {
  if (!is.factor(x))
    x <- factor(x)

  xtab <- sort(table(x, useNA = "no"), decreasing = TRUE)

  if (threshold < 1) {
    xtab <- xtab / sum(!is.na(x))
  }

  dropped <- which(xtab < threshold)
  orig <- levels(x)

  if (length(dropped) > 0)
    keepers <- names(xtab[-dropped])
  else
    keepers <- orig

  if (length(keepers) == 0)
    keepers <- names(xtab)[which.max(xtab)]

  if (other %in% keepers)
    rlang::abort(
        paste0(
        "The level ",
        other,
        " is already a factor level that will be retained. ",
        "Please choose a different value."
      )
    )

  list(keep = orig[orig %in% keepers],
       collapse = length(dropped) > 0,
       other = other)
}


#' @rdname tidy.recipe
#' @export
tidy.step_other <- function(x, ...) {
  if (is_trained(x)) {
    values <- purrr::map(x$objects, function(x) x$keep)
    n <- vapply(values, length, integer(1))
    values <- vctrs::vec_unchop(values, ptype = character(), name_spec = rlang::zap())
    res <- tibble(terms = rep(names(n), n),
                  retained = values)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  retained = rep(na_chr, length(term_names)))
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

