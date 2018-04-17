#' Dummy Variables Creation
#'
#' `step_dummy` creates a a *specification* of a recipe
#'  step that will convert nominal data (e.g. character or factors)
#'  into one or more numeric binary model terms for the levels of
#'  the original data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to create the dummy variables. See
#'  [selections()] for more details. The selected
#'  variables must be factors. For the `tidy` method, these are
#'  not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the binary dummy variable columns created by the original
#'  variables will be used as predictors in a model.
#' @param one_hot A logical. For C levels, should C dummy variables be created
#' rather than C-1?
#' @param naming A function that defines the naming convention for
#'  new dummy columns. See Details below.
#' @param levels A list that contains the information needed to
#'  create dummy variables for each variable contained in
#'  `terms`. This is `NULL` until the step is trained by
#'  [prep.recipe()].
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected).
#' @keywords datagen
#' @concept preprocessing dummy_variables model_specification
#'  dummy_variables variable_encodings
#' @export
#' @details `step_dummy` will create a set of binary dummy
#'  variables from a factor variable. For example, if an unordered
#'  factor column in the data set has levels of "red", "green",
#'  "blue", the dummy variable bake will create two additional
#'  columns of 0/1 data for two of those three values (and remove
#'  the original column). For ordered factors, polynomial contrasts
#'  are used to encode the numeric values.
#'
#' By default, the missing dummy variable (i.e. the reference
#'  cell) will correspond to the first level of the unordered
#'  factor being converted.
#'
#' The function allows for non-standard naming of the resulting
#'  variables. For an unordered factor named `x`, with levels `"a"`
#'  and `"b"`, the default naming convention would be to create a
#'  new variable called `x_b`. Note that if the factor levels are
#'  not valid variable names (e.g. "some text with spaces"), it will
#'  be changed by [base::make.names()] to be valid (see the example
#'  below). The naming format can be changed using the `naming`
#'  argument and the function [dummy_names()] is the default. This
#'  function will also change the names of ordinal dummy variables.
#'  Instead of values such as "`.L`", "`.Q`", or "`^4`", ordinal
#'  dummy variables are given simple integer suffixes such as
#'  "`_1`", "`_2`", etc.
#'
#' To change the type of contrast being used, change the global contrast option
#'  via `options`.
#'
#' The [package vignette for dummy variables](https://topepo.github.io/recipes/articles/Dummies.html)
#' and interactions has more information.
#'
#' @seealso [step_factor2string()], [step_string2factor()],
#'  [dummy_names()], [step_regex()], [step_count()],
#'  [step_ordinalscore()], [step_unorder()], [step_other()]
#'  [step_novel()]
#' @examples
#' data(okc)
#' okc <- okc[complete.cases(okc),]
#'
#' rec <- recipe(~ diet + age + height, data = okc)
#'
#' dummies <- rec %>% step_dummy(diet)
#' dummies <- prep(dummies, training = okc)
#'
#' dummy_data <- bake(dummies, newdata = okc)
#'
#' unique(okc$diet)
#' grep("^diet", names(dummy_data), value = TRUE)
#'
#' # Obtain the full set of dummy variables using `one_hot` option
#' rec %>%
#'   step_dummy(diet, one_hot = TRUE) %>%
#'   prep(training = okc, retain = TRUE) %>%
#'   juice(starts_with("diet")) %>%
#'   names() %>%
#'   length()
#'
#' length(unique(okc$diet))
#'
#' # Without one_hot
#' length(grep("^diet", names(dummy_data), value = TRUE))
#'
#'
#' tidy(dummies, number = 1)


step_dummy <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           one_hot = FALSE,
           naming = dummy_names,
           levels = NULL,
           skip = FALSE) {
    add_step(
      recipe,
      step_dummy_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        one_hot = one_hot,
        naming = naming,
        levels = levels,
        skip = skip
      )
    )
  }

step_dummy_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           one_hot = one_hot,
           naming = naming,
           levels = levels,
           skip = FALSE
  ) {
    step(
      subclass = "dummy",
      terms = terms,
      role = role,
      trained = trained,
      one_hot = one_hot,
      naming = naming,
      levels = levels,
      skip = skip
    )
  }

#' @importFrom stats as.formula model.frame
#' @importFrom dplyr bind_cols
#' @export
prep.step_dummy <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  fac_check <-
    vapply(training[, col_names], is.factor, logical(1))
  if (any(!fac_check))
    stop(
      "The following variables are not factor vectors: ",
      paste0("`", names(fac_check)[!fac_check], "`", collapse = ", "),
      call. = FALSE
    )


  ## I hate doing this but currently we are going to have
  ## to save the terms object form the original (= training)
  ## data
  levels <- vector(mode = "list", length = length(col_names))
  names(levels) <- col_names
  for (i in seq_along(col_names)) {
    form_chr <- paste0("~", col_names[i])
    if(x$one_hot) {
      form_chr <- paste0(form_chr, "-1")
    }
    form <- as.formula(form_chr)
    terms <- model.frame(form,
                         data = training,
                         xlev = x$levels[[i]])
    levels[[i]] <- attr(terms, "terms")

    ## About factor levels here: once dummy variables are made,
    ## the `stringsAsFactors` info saved in the recipe (under
    ## recipe$levels will remove the original record of the
    ## factor levels at the end of `prep.recipe` since it is
    ## not a factor anymore. We'll save them here and reset them
    ## in `bake.step_dummy` just prior to calling `model.matrix`
    attr(levels[[i]], "values") <-
      levels(getElement(training, col_names[i]))
  }

  step_dummy_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    one_hot = x$one_hot,
    naming = x$naming,
    levels = levels,
    skip = x$skip
  )
}

#' @export
bake.step_dummy <- function(object, newdata, ...) {
  ## Maybe do this in C?
  col_names <- names(object$levels)

  ## `na.action` cannot be passed to `model.matrix` but we
  ## can change it globally for a bit
  old_opt <- options()$na.action
  options(na.action = "na.pass")
  on.exit(options(na.action = old_opt))

  for (i in seq_along(object$levels)) {
    # Make sure that the incoming data has levels consistent with
    # the original (see the note above)
    orig_var <- names(object$levels)[i]
    fac_type <- attr(object$levels[[i]], "dataClasses")

    if(!any(names(attributes(object$levels[[i]])) == "values"))
      stop("Factor level values not recorded", call. = FALSE)

    newdata[, orig_var] <-
      factor(getElement(newdata, orig_var),
             levels = attr(object$levels[[i]], "values"),
             ordered = fac_type == "ordered")

    indicators <-
      model.matrix(
        object = object$levels[[i]],
        data = newdata
      )

    options(na.action = old_opt)
    on.exit(expr = NULL)

    if(!object$one_hot) {
      indicators <- indicators[, colnames(indicators) != "(Intercept)", drop = FALSE]
    }

    ## use backticks for nonstandard factor levels here
    used_lvl <- gsub(paste0("^", col_names[i]), "", colnames(indicators))
    colnames(indicators) <- object$naming(col_names[i], used_lvl, fac_type == "ordered")
    newdata <- bind_cols(newdata, as_tibble(indicators))
    newdata[, col_names[i]] <- NULL
  }
  if (!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}

print.step_dummy <-
  function(x, width = max(20, options()$width - 20), ...) {
    if (x$trained) {
      cat("Dummy variables from ")
      cat(format_ch_vec(names(x$levels), width = width))
    } else {
      cat("Dummy variables from ", sep = "")
      cat(format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_dummy
#' @param x A `step_dummy` object.
tidy.step_dummy <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$levels))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}
