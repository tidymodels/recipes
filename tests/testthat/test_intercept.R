library(testthat)
library(recipes)
library(tibble)

ex_dat <- data.frame(cat = rep(c("A", "B"), each = 5), numer = 1:10)

test_that('add appropriate column with default settings', {
  rec <- recipe(~ ., data = ex_dat) %>%
    step_intercept()

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, newdata = ex_dat)

  exp_res <- tibble::add_column(ex_dat, "intercept" = 1, .before = TRUE)

  expect_equal(rec_trans, exp_res)
})

test_that('adds arbitrary numeric column', {
  rec <- recipe(~ ., data = ex_dat) %>%
    step_intercept(name = "(Intercept)", value = 2.5)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, newdata = ex_dat)

  exp_res <- tibble::add_column(ex_dat, "(Intercept)" = 2.5, .before = TRUE)

  expect_equal(rec_trans, exp_res)
})


test_that('deals with bad input', {
  expect_error(
    recipe(~ ., data = ex_dat) %>%
      step_intercept(value = "Pie") %>%
      prep(),
    "Intercept value must be numeric."
  )

  expect_error(
    recipe(~ ., data = ex_dat) %>%
      step_intercept(name = 4) %>%
      prep(),
    "Intercept/constant column name must be a character value."
  )

  expect_warning(
    recipe(~ ., data = ex_dat) %>%
      step_intercept(all_predictors()) %>%
      prep(),
    "Selectors are not used for this step."
  )
})

test_that('printing', {
  rec <- recipe(~ ., data = ex_dat) %>%
    step_intercept()
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})

