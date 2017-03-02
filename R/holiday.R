#' Holiday Feature Generator
#' 
#' \code{step_holiday} creates a a \emph{specification} of a recipe step that will convert date data into one or more binary indicator variables for common holidays. 
#' 
#' @inheritParams step_center
#' @param terms A representation of the variables or terms that will be used to create the new variables. The selected variables should have class \code{Date} or \code{POSIXct}.
#' @param role For model terms created by this step, what analysis role should they be assigned?. By default, the function assumes that the new variable columns created by the original variables will be used as predictors in a model. 
#' @param holidays A character string that includes at least one holdiay supported by the \code{timeDate} package. See \code{\link[timeDate]{listHolidays}} for a complete list.  

#' @param variables A character string of variables that will be used as inputs. This field is a placeholder and will be populated once \code{\link{learn.recipe}} is used. 
#' @return \code{step_holiday} returns an object of class \code{step_holiday}.
#' @keywords datagen
#' @concept preprocessing model_specification variable_encodings dates
#' @export
#' @details Unlike other steps, \code{step_holiday} does \emph{not} remove the original date variables. \code{\link{step_rm}} can be used for this purpose. 
#' @examples 
#' library(lubridate)
#' library(magrittr)
#' examples <- data.frame(someday = ymd("2000-12-20") + days(0:40))
#' holiday_rec <- recipe(~ someday, examples) %>%
#'    step_holiday(~ is_predictor())
#'
#' holiday_rec <- learn(holiday_rec, training = examples)   
#' holiday_values <- process(holiday_rec, newdata = examples)
#' holiday_values                                               
#' @seealso \code{\link{step_date}} \code{\link{step_rm}} \code{\link{recipe}} \code{\link{learn.recipe}} \code{\link{process.recipe}} \code{\link[timeDate]{listHolidays}}
#' @import timeDate 
step_holiday <- function(recipe, 
                      terms, 
                      role = "predictor",
                      trained = FALSE, 
                      holidays = c("LaborDay", "NewYearsDay", "ChristmasDay"),
                      variables = NULL) {
  all_days <- listHolidays()
  if(!all(holidays %in% all_days)) 
    stop("Invalid `holidays` value. See timeDate::listHolidays")
  
  add_step(
    recipe, 
    step_holiday_new(
      terms = terms, 
      role = role,
      trained = trained,
      holidays = holidays, 
      variables = variables))
}

step_holiday_new <- function(terms = NULL, 
                          role = "predictor",
                          trained = FALSE,
                          holidays = holidays, 
                          variables = variables) {
  step(
    subclass = "holiday", 
    terms = terms,
    role = role,
    trained = trained,
    holidays = holidays,
    variables = variables
  )
}

#' @importFrom stats as.formula model.frame
learn.step_holiday <- function(x, training, info = NULL, ...) {
  col_names <- parse_terms_formula(x$terms, info = info) 
  
  holiday_data <- info[info$variable %in% col_names,]
  if(any(holiday_data$type != "date")) 
    stop("All variables for `step_holiday` should be either `Date` or `POSIXct` classes.")
  
  step_holiday_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    holidays = x$holidays,
    variables = col_names
  )
}


is_holiday <- function(hol, dt) {
  hdate <- holiday(year = unique(year(dt)), Holiday = hol)
  hdate <- as.Date(hdate)
  out <- rep(0, length(dt))
  out[dt %in% hdate] <- 1
  out
}

#' @importFrom lubridate year is.Date
get_holiday_features <- function(dt, hdays) {
  if(!is.Date(dt)) dt <- as.Date(dt)
  hdays <- as.list(hdays)
  hfeat <- lapply(hdays, is_holiday, dt = dt)
  hfeat <- do.call("cbind", hfeat)
  colnames(hfeat) <- unlist(hdays)
  as_tibble(hfeat)
}

#' @importFrom tibble as_tibble is_tibble
process.step_holiday <- function(object, newdata, ...) {
  new_cols <- rep(length(object$holidays), each = length(object$variables))
  holiday_values <- matrix(NA, nrow = nrow(newdata), ncol = sum(new_cols))
  colnames(holiday_values) <- rep("", sum(new_cols))
  holiday_values <- as_tibble(holiday_values)
  
  strt <- 1
  for(i in seq_along(object$variables)) {
    cols <- (strt):(strt+new_cols[i]-1)

    tmp <- get_holiday_features(
      dt = getElement(newdata, object$variables[i]),
      hdays = object$holidays
    )
    
    holiday_values[, cols] <- tmp
      
    names(holiday_values)[cols] <- 
      paste(
        object$variables[i],
        names(tmp),
        sep = "_"
      )
    
    strt <- max(cols)+1
    # newdata[, object$variables[i] ] <- NULL
  }
  newdata <- cbind(newdata, holiday_values)
  if(!is_tibble(newdata)) as_tibble(newdata)
}

print.step_holiday <- function(x, width = 30, ...) {
  cat("Holiday indicators from ")
  cat(format_formula(x$terms, wdth = width))
  if(x$trained) cat(" [trained]\n") else cat("\n")
  invisible(x)
}
