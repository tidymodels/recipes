library(testthat)
library(recipes)

data(okc)

okc$diet[is.na(okc$diet)] <- "missing"
okc <- okc[complete.cases(okc), -5]

okc_fac <- okc
okc_fac$diet <- factor(okc_fac$diet)
okc_fac$location <- factor(okc_fac$location)

test_that('dummy variables with factor inputs', {
  rec <- recipe(age ~ location + diet, data = okc_fac)
  dummy <- rec %>% step_dummy(diet, location)
  dummy_trained <- prep(dummy, training = okc_fac, verbose = FALSE, stringsAsFactors = FALSE)
  dummy_pred <- bake(dummy_trained, newdata = okc_fac, all_predictors())
  dummy_pred <- dummy_pred[, order(colnames(dummy_pred))]
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  exp_res <- model.matrix(age ~ location + diet, data = okc_fac)[, -1]
  exp_res <- exp_res[, colnames(exp_res) != "age"]
  colnames(exp_res) <- gsub("^location", "location_", colnames(exp_res))
  colnames(exp_res) <- gsub("^diet", "diet_", colnames(exp_res))
  colnames(exp_res) <- make.names(colnames(exp_res))
  exp_res <- exp_res[, order(colnames(exp_res))]
  exp_res <- as.data.frame(exp_res)
  rownames(exp_res) <- NULL
  expect_equal(dummy_pred, exp_res)

  dum_tibble <-
    tibble(terms = c("diet", "location"))

  expect_equal(tidy(dummy, 1), dum_tibble)
  expect_equal(tidy(dummy_trained, 1), dum_tibble)
})

test_that('dummy variables with string inputs', {
  rec <- recipe(age ~ location + diet, data = okc)
  dummy <- rec %>% step_dummy(diet, location)
  expect_error(
    prep(dummy, training = okc, verbose = FALSE, stringsAsFactors = FALSE)
  )
})

test_that('create all dummy variables', {
  rec <- recipe(age ~ location + diet + height, data = okc_fac)
  dummy <- rec %>% step_dummy(diet, location, one_hot = TRUE)
  dummy_trained <- prep(dummy, training = okc_fac, verbose = FALSE, stringsAsFactors = FALSE)
  dummy_pred <- bake(dummy_trained, newdata = okc_fac, all_predictors())
  dummy_pred <- dummy_pred[, order(colnames(dummy_pred))]
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  exp_res <- NULL
  for(pred in c("diet", "height", "location")) {
    tmp <- model.matrix(as.formula(paste("~", pred, "+ 0")), data = okc_fac)
    colnames(tmp) <- gsub(paste0("^", pred), paste0(pred, "_"), colnames(tmp))
    exp_res <- bind_cols(exp_res, as_tibble(tmp))
  }
  colnames(exp_res) <- make.names(colnames(exp_res))
  exp_res <- as.data.frame(exp_res)
  rownames(exp_res) <- NULL
  expect_equivalent(dummy_pred, exp_res)
})

test_that('tests for issue #91', {
  rec <- recipe(~ diet, data = okc)
  factors <- rec %>% step_dummy(diet)
  factors <- prep(factors, training = okc)
  factors_data_1 <- bake(factors, newdata = okc)
  # Remove one category in diet
  factors_data_2 <- bake(factors, newdata = okc %>% filter(diet != 'halal'))
  expect_equal(names(factors_data_1), names(factors_data_2))

  # now with ordered factor

  okc$ordered_diet <- as.ordered(okc$diet)
  rec <- recipe(~ ordered_diet, data = okc)
  orderedfac <- rec %>% step_dummy(ordered_diet)
  orderedfac <- prep(orderedfac, training = okc)
  ordered_data_1 <- bake(orderedfac, newdata = okc)
  # Remove one category in diet
  ordered_data_2 <- bake(orderedfac, newdata = okc %>% filter(diet != 'halal'))
  expect_equal(names(ordered_data_1), names(ordered_data_2))

})

test_that('naming function', {
  expect_equal(dummy_names("x", letters[1:3]), c("x_a", "x_b", "x_c"))
  expect_equal(dummy_names("x", letters[1:3], ordinal = TRUE),
               c("x_1", "x_2", "x_3"))
})

test_that('printing', {
  rec <- recipe(age ~ ., data = okc_fac)
  dummy <- rec %>% step_dummy(diet, location)
  expect_output(print(dummy))
  expect_output(prep(dummy, training = okc_fac, verbose = TRUE))
})

