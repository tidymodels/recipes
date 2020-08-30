#' Convert values to predefined integers
#'
#' `step_integer` creates a *specification* of a recipe
#'  step that will convert new data into a set of integers based
#'  on the original data values.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to create the integer variables. See
#'  [selections()] for more details. For the `tidy` method, these
#'  are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new columns created by the original variables will be
#'  used as predictors in a model.
#' @param key A list that contains the information needed to
#'  create integer variables for each variable contained in
#'  `terms`. This is `NULL` until the step is trained by
#'  [prep.recipe()].
#' @param strict A logical for whether the values should be returned as
#'  integers (as opposed to double).
#' @param zero_based A logical for whether the integers should start at zero and
#'  new values be appended as the largest integer.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any). For the `tidy`
#'  method, a tibble with columns `terms` (the selectors or
#'  variables selected) and `value` is a _list column_ with the
#'  conversion key.
#' @keywords datagen
#' @concept preprocessing
#' @concept variable_encodings
#' @export
#' @details `step_integer` will determine the unique values of
#'  each variable from the training set (excluding missing values),
#'  order them, and then assign integers to each value. When baked,
#'  each data point is translated to its corresponding integer or a
#'  value of zero for yet unseen data (although see the `zero_based`
#'  argument above). Missing values propagate.
#'
#' Factor inputs are ordered by their levels. All others are
#'  ordered by `sort`.
#'
#' Despite the name, the new values are returned as numeric unless
#'  `strict = TRUE`, which will coerce the results to integers.
#'
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_other()]
#'  [step_novel()], [step_dummy()]
#' @examples
#' library(modeldata)
#' data(okc)
#'
#' okc$location <- factor(okc$location)
#'
#' okc_tr <- okc[1:100, ]
#' okc_tr$age[1] <- NA
#'
#' okc_te <- okc[101:105, ]
#' okc_te$age[1] <- NA
#' okc_te$diet[1] <- "fast food"
#' okc_te$diet[2] <- NA
#'
#' rec <- recipe(Class ~ ., data = okc_tr) %>%
#'   step_integer(all_predictors()) %>%
#'   prep(training = okc_tr)
#'
#' bake(rec, okc_te, all_predictors())
#' tidy(rec, number = 1)

step_integer <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           strict = FALSE,
           zero_based = FALSE,
           key = NULL,
           skip = FALSE,
           id = rand_id("integer")) {
    add_step(
      recipe,
      step_integer_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        strict = strict,
        zero_based = zero_based,
        key = key,
        skip = skip,
        id = id
      )
    )
  }

step_integer_new <-
  function(terms, role, trained, strict, zero_based, key, skip, id) {
    step(
      subclass = "integer",
      terms = terms,
      role = role,
      trained = trained,
      strict = strict,
      zero_based = zero_based,
      key = key,
      skip = skip,
      id = id
    )
  }

get_unique_values <- function(x, zero = FALSE) {
  if(is.factor(x)) {
    res <- levels(x)
  } else {
    res <- sort(unique(x))
  }
  res <- res[!is.na(res)]
  ints <- seq_along(res)
  if (zero) {
    ints <- ints - 1
  }
  tibble(value = res, integer = ints)
}

#' @export
prep.step_integer <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  step_integer_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    strict = x$strict,
    zero_based = x$zero_based,
    key = map(training[, col_names], get_unique_values, zero = x$zero_based),
    skip = x$skip,
    id = x$id
  )
}

map_key_to_int <- function(dat, key, strict = FALSE, zero = FALSE) {
  if (is.factor(dat))
    dat <- as.character(dat)

  res <- full_join(tibble(value = dat, .row = seq_along(dat)), key, by = "value")
  res <- dplyr::filter(res, !is.na(.row))
  res <- arrange(res, .row)
  if (zero) {
    res$integer[is.na(res$integer) & !is.na(res$value)] <-
      max(key$integer, na.rm = TRUE) + 1
  } else {
    res$integer[is.na(res$integer) & !is.na(res$value)] <- 0
  }
  if (strict)
    res$integer <- as.integer(res$integer)
  res[["integer"]]
}

#' @export
bake.step_integer <- function(object, new_data, ...) {

  for (i in names(object$key)) {
    new_data[[i]] <-
      map_key_to_int(new_data[[i]], object$key[[i]], object$strict, object$zero_based)
  }
  if (!is_tibble(new_data))
    new_data <- as_tibble(new_data)
  new_data
}

print.step_integer <-
  function(x, width = max(20, options()$width - 20), ...) {
    if (x$trained) {
      cat("Integer encoding for ")
      cat(format_ch_vec(names(x$key), width = width))
    } else {
      cat("Integer encoding for ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_integer
#' @param x A `step_integer` object.
#' @export
tidy.step_integer <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$key), value = x$key)
  } else {
    res <- tibble(terms = sel2char(x$terms))
    res$value = NA
  }
  res$id <- x$id
  res
}
