library(testthat)
library(recipes)

ex_dat <- tibble(
  cat = factor(rep(c("A", "B"), each = 5)),
  numer = 1:10
)

test_that("add appropriate column with default settings", {
  rec <- recipe(~., data = ex_dat) |>
    step_intercept()

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- tibble::add_column(ex_dat, "intercept" = 1L, .before = TRUE)

  expect_identical(rec_trans, exp_res)
})

test_that("adds arbitrary numeric column", {
  rec <- recipe(~., data = ex_dat) |>
    step_intercept(name = "(Intercept)", value = 2.5)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- tibble::add_column(ex_dat, "(Intercept)" = 2.5, .before = TRUE)

  expect_identical(rec_trans, exp_res)
})

test_that("deals with bad input", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = ex_dat) |>
      step_intercept(value = "Pie") |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = ex_dat) |>
      step_intercept(name = 4) |>
      prep()
  )

  expect_snapshot(
    recipe(~., data = ex_dat) |>
      step_intercept(all_predictors()) |>
      prep()
  )
})

test_that("check_name() is used", {
  dat <- mtcars
  dat$intercept <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_intercept()

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_intercept() is special as it doesn't care about the incoming data
  expect_true(TRUE)
})

test_that("empty printing", {
  # Here for completeness
  # step_intercept() is special as it can't be used without selection
  expect_true(TRUE)
})

test_that("empty selection prep/bake is a no-op", {
  # Here for completeness
  # step_intercept() is special as it can't be used without selection
  expect_true(TRUE)
})

test_that("empty selection tidy method works", {
  # Here for completeness
  # step_intercept() is special as it can't be used without selection
  expect_true(TRUE)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_intercept()

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_intercept() |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
