library(testthat)
library(magrittr)
library(recipes)

data(okc)

okc$location <- gsub(", california", "", okc$location)
okc$diet[is.na(okc$diet)] <- "missing"
okc <- okc[complete.cases(okc), -5]

okc_fac <- data.frame(okc)

test_that('dummy variables with string inputs', {
  rec <- recipe(age ~ ., data = okc)
  dummy <- rec %>% step_dummy(~ diet  + location)
  dummy_trained <- learn(dummy, training = okc, verbose = FALSE, stringsAsFactors = FALSE)
  dummy_pred <- process(dummy_trained, newdata = okc, roles = "predictor")
  dummy_pred <- dummy_pred[, order(colnames(dummy_pred))]
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL
  
  exp_res <- model.matrix(age ~ ., data = okc_fac)[, -1]
  exp_res <- exp_res[, colnames(exp_res) != "age"]
  colnames(exp_res) <- gsub("^location", "location_", colnames(exp_res))
  colnames(exp_res) <- gsub("^diet", "diet_", colnames(exp_res))
  colnames(exp_res) <- make.names(colnames(exp_res))
  exp_res <- exp_res[, order(colnames(exp_res))]
  exp_res <- as.data.frame(exp_res)
  rownames(exp_res) <- NULL
  expect_equal(dummy_pred, exp_res)
})
