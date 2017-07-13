library(testthat)
library(recipes)
library(tibble)

ex_dat <- data.frame(cat = rep(c("A", "B"), each = 5), numer = 1:10)

test_that('adds column of ones', {
  rec <- recipe(~ ., data = ex_dat) %>%
    step_intercept()

  rec_trained <- prepare(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, newdata = ex_dat)

  exp_res <- tibble::add_column(ex_dat, intercept = 1, .before = TRUE)

  expect_equal(rec_trans, exp_res)
})

test_that('adds arbitrary numeric column', {
  rec <- recipe(~ ., data = ex_dat) %>%
    step_intercept(value = 2.5)

  rec_trained <- prepare(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, newdata = ex_dat)

  exp_res <- tibble::add_column(ex_dat, intercept = 2.5, .before = TRUE)

  expect_equal(rec_trans, exp_res)
})


test_that('deals with bad input', {
  expect_error(
    recipe(~ ., data = ex_dat) %>%
      step_intercept(value = "Pie") %>%
      prepare(),
    "Intercept value must be numeric."
  )

  expect_warning(
    recipe(~ ., data = ex_dat) %>%
      step_intercept(all_predictors()) %>%
      prepare(),
    "Term arguments passed to step_intercept have no effect."
  )

  ex_dat2 <- data.frame(numer = rep(1, 10))

  rec_trained <- recipe(~ ., data = ex_dat2) %>%
    step_intercept() %>%
    prepare(training = ex_dat2, verbose = FALSE)

  expect_message(
    bake(rec_trained, newdata = ex_dat2),
    "Data appears to already contain intercept column."
  )
})

test_that('printing', {
  rec <- recipe(~ ., data = ex_dat) %>%
    step_intercept()
  expect_output(print(rec))
  expect_output(prepare(rec, training = ex_dat))
})

