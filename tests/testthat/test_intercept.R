library(testthat)
library(recipes)
library(tibble)

ex_dat <- tibble(
  cat = factor(rep(c("A", "B"), each = 5)),
  numer = 1:10
)

test_that("add appropriate column with default settings", {
  rec <- recipe(~., data = ex_dat) %>%
    step_intercept()

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- tibble::add_column(ex_dat, "intercept" = 1, .before = TRUE)

  expect_equal(rec_trans, exp_res)
})

test_that("adds arbitrary numeric column", {
  rec <- recipe(~., data = ex_dat) %>%
    step_intercept(name = "(Intercept)", value = 2.5)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- tibble::add_column(ex_dat, "(Intercept)" = 2.5, .before = TRUE)

  expect_equal(rec_trans, exp_res)
})


test_that("deals with bad input", {
  expect_snapshot(error = TRUE,
    recipe(~., data = ex_dat) %>%
      step_intercept(value = "Pie") %>%
      prep()
  )

  expect_snapshot(error = TRUE,
    recipe(~., data = ex_dat) %>%
      step_intercept(name = 4) %>%
      prep()
  )

  expect_snapshot(
    recipe(~., data = ex_dat) %>%
      step_intercept(all_predictors()) %>%
      prep()
  )
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) %>%
    step_intercept()
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
