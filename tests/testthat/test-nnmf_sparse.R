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
  skip_if_not_installed("RcppML")
  library(Matrix)
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_nnmf_sparse(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  skip_if_not_installed("RcppML")
  library(Matrix)
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_nnmf_sparse(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("printing", {
  skip_if_not_installed("RcppML")
  library(Matrix)
  rec <- recipe(mpg ~ ., mtcars) %>%
    step_nnmf_sparse(disp, drat)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
