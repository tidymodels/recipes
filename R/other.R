#' Collapse Some Categorical Levels
#'
#' \code{step_other} creates a \emph{specification} of a recipe step that will potentially pool infrequently occurring values into an "other" category.  
#'
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will potentially be reduced.
#' @param role Not used by this step since no new variables are created.
#' @param threshold A single numeric value in (0, 1) for pooling. 
#' @param other A single character value for the "other" category. 
#' @param objects A list of objects that contain the information to pool infrequent levels that is determined by \code{\link{learn.recipe}}.
#' @return \code{step_other}  returns an object of class \code{step_other}.
#' @keywords datagen
#' @concept preprocessing normalization_methods factors
#' @export
#' @details The overall proportion of the categories are computed and sorted. The "other" category is used for categorical levels whose cumulative proportion is less than \code{threshold}. 
#' 
#' If no pooling is done the variable is unmodified. Otherwise, a factor is always returned with different factor levels. 
#' 
#' If \code{threshold} is less than the largest category proportion, all levels except for the most frequent are collapsed to the \code{other} level. 
#' 
#' If the retained categories include the value of \code{other}, an error is thrown. If \code{other} is in the list of discarded levels, no error occurs. 
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
#' library(magrittr)
#' rec <- rec %>% step_other(~ diet + location, threshold = .8, other = "other values")
#' rec <- learn(rec, training = okc_tr)
#' 
#' collapsed <- process(rec, okc_te)
#' table(okc_te$diet, collapsed$diet, useNA = "always")

step_other <- function(recipe, terms, role = NA, trained = FALSE, threshold = .95, other = "other", objects = NULL) {
  if(threshold <= 0)
    stop("`threshold` should be greater than zero")
  if(threshold >= 1)
    stop("`threshold` should be less than one")
  
  add_step(
    recipe,
    step_other_new(
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      other = other,
      objects = objects
    )
  )
}

step_other_new <- function(terms = NULL, role = NA, trained = FALSE, threshold = NULL, other = NULL, objects = NULL) {
  step(
    subclass = "other",
    terms = terms,
    role = role,
    trained = trained,
    threshold = threshold,
    other = other,
    objects = objects
  )
}

#' @importFrom stats sd
learn.step_other <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info)
  objects <- lapply(training[, col_names], keep_levels,
                    prop = x$threshold, other = x$other)
  step_other_new(terms = x$terms, role = x$role, trained = TRUE,
                 threshold = x$threshold,
                 other = x$other,
                 objects = objects)
}

#' @importFrom tibble as_tibble is_tibble
process.step_other <- function(object, newdata, ...) {
  for(i in names(object$objects)) {
    if(object$objects[[i]]$collapse) {
      tmp <- if(!is.character(newdata[, i]))
        as.character(getElement(newdata, i)) else 
          getElement(newdata, i)
      
      tmp <- ifelse(tmp %in% object$objects[[i]]$keep,
                    tmp, object$objects[[i]]$other)
      tmp <- factor(tmp,
                    levels = c(object$objects[[i]]$keep,
                               object$objects[[i]]$other))
      tmp[is.na(getElement(newdata, i))] <- NA
      newdata[, i] <- tmp
    }
  }
  if(!is_tibble(newdata))
    newdata <- as_tibble(newdata)
  newdata
}

print.step_other <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Collapsing factor levels for ", sep = "")
  if(x$trained) {
    cat(format_ch_vec(names(x$objects), width = width))
  } else cat(format_formula(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}


keep_levels <- function(x, prop = .9, other = "other") {
  if(!is.factor(x)) x <- factor(x)
  xtab <- sort(table(x, useNA = "no"), decreasing = TRUE)/sum(!is.na(x))
  keepers <- if(prop < max(xtab)) 
    names(xtab)[1] else 
      names(which(cumsum(xtab) <= prop))
  orig <- levels(x)
  if(other %in% keepers)
    stop("The level", other, "is already a factor level that will be retained. ",
         "Please choose a different value.")
  list(keep = orig[orig %in% keepers],
       collapse = length(orig) != length(keepers),
       other = other)
}

