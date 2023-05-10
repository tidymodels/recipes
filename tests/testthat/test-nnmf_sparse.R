test_that("check_name() is used", {
  skip_if_not_installed("RcppML")
  data("ames", package = "modeldata")
  library(Matrix)
  dat <- mtcars
  dat$NNMF1 <- as.character(dat$mpg)

  rec <- recipe(~ ., data = dat) %>%
    step_nnmf_sparse(all_numeric_predictors())

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_nnmf_sparse(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("printing", {
  library(Matrix)
  rec <- recipe(mpg ~ ., mtcars) %>%
    step_nnmf_sparse(disp, drat)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
