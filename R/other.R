#' Collapse Some Categorical Levels
#'
#' `step_other` creates a *specification* of a recipe
#'  step that will potentially pool infrequently occurring values
#'  into an "other" category.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables that will potentially be reduced. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param threshold A single numeric value between 0 (inclusive)
#'  and 1 for pooling. Factor levels whose rate of occurrence in
#'  the training set are below `threshold` will be "othered".
#' @param other A single character value for the "other" category.
#' @param objects A list of objects that contain the information
#'  to pool infrequent levels that is determined by
#'  [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected) and `retained` (the factor
#'  levels that were not pulled into "other")
#' @keywords datagen
#' @concept preprocessing
#' @concept factors
#' @export
#' @details The overall proportion of the categories are computed. The "other"
#'   category is used in place of any categorical levels whose individual
#'   proportion in the training set is less than `threshold`.
#'
#' If no pooling is done the data are unmodified (although character data may
#'   be changed to factors based on the value of `strings_as_factors` in
#'   [prep.recipe()]). Otherwise, a factor is always returned with
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
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [dummy_names()], [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_novel()]
#' @examples
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
    if (threshold <= 0)
      stop("`threshold` should be greater than zero", call. = FALSE)
    if (threshold >= 1)
      stop("`threshold` should be less than one", call. = FALSE)
    add_step(
      recipe,
      step_other_new(
        terms = ellipse_check(...),
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

#' @importFrom stats sd
#' @export
prep.step_other <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info, empty_fun = passover)

  if (length(col_names) > 0) {
    objects <- lapply(training[, col_names],
                      keep_levels,
                      prop = x$threshold,
                      other = x$other)
  } else {
    objects <- NULL
  }

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

#' @importFrom tibble as_tibble is_tibble
#' @export
bake.step_other <- function(object, new_data, ...) {
  if (!is.null(object$objects)) {
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
  }
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_other <-
  function(x, width = max(20, options()$width - 30), ...) {

    if (x$trained) {
      collapsed <- map_lgl(x$objects, ~ .x$collapse)
      collapsed <- names(collapsed)[collapsed]
      if (length(collapsed) > 0) {
        cat("Collapsing factor levels for ", sep = "")
        printer(collapsed, x$terms, x$trained, width = width)
      } else {
        cat("No factor levels were collapsed\n")
      }
    } else {
      cat("Collapsing factor levels for ", sep = "")
      printer(names(x$objects), x$terms, x$trained, width = width)
    }
    invisible(x)
  }

keep_levels <- function(x, prop = .1, other = "other") {
  if (!is.factor(x))
    x <- factor(x)
  xtab <-
    sort(table(x, useNA = "no"), decreasing = TRUE) / sum(!is.na(x))
  dropped <- which(xtab < prop)
  orig <- levels(x)

  if (length(dropped) > 0)
    keepers <- names(xtab[-dropped])
  else
    keepers <- orig

  if (length(keepers) == 0)
    keepers <- names(xtab)[which.max(xtab)]

  if (other %in% keepers)
    stop(
      "The level ",
      other,
      " is already a factor level that will be retained. ",
      "Please choose a different value.", call. = FALSE
    )

  list(keep = orig[orig %in% keepers],
       collapse = length(dropped) > 0,
       other = other)
}


#' @rdname step_other
#' @param x A `step_other` object.
#' @importFrom purrr map
#' @export
tidy.step_other <- function(x, ...) {
  if (is_trained(x)) {
    values <- purrr::map(x$objects, function(x) x$keep)
    n <- vapply(values, length, integer(1))
    res <- tibble(terms = rep(names(n), n),
                  retained = unname(unlist(values)))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  retained = rep(na_chr, length(term_names)))
  }
  res$id <- x$id
  res
}
