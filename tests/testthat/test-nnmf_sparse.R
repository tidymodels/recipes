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

test_that("Do nothing for num_comps = 0 and keep_original_cols = FALSE (#1152)", {
  skip_if_not_installed("RcppML")
  library(Matrix)

  rec <- recipe(~ ., data = mtcars) %>%
    step_nnmf_sparse(all_predictors(), num_comp = 0, keep_original_cols = FALSE) %>%
    prep()

  res <- bake(rec, new_data = NULL)

  expect_identical(res, tibble::as_tibble(mtcars))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("RcppML")
  library(Matrix)

  rec <- recipe(mtcars) %>%
    step_nnmf_sparse(disp, wt) %>%
    update_role(disp, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = mtcars)

  expect_error(bake(rec_trained, new_data = mtcars[, -3]),
               class = "new_data_missing_column")
})

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

test_that("empty selection tidy method works", {
  skip_if_not_installed("RcppML")
  library(Matrix)
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_nnmf_sparse(rec)

  expect <- tibble(
    terms = character(),
    value = double(),
    component = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("RcppML")
  library(Matrix)
  new_names <- c("NNMF1")

  rec <- recipe(~ mpg, mtcars) %>%
    step_nnmf_sparse(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~ mpg, mtcars) %>%
    step_nnmf_sparse(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("mpg", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("RcppML")
  library(Matrix)
  rec <- recipe(~ mpg, mtcars) %>%
    step_nnmf_sparse(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_error(
    bake(rec, new_data = mtcars),
    NA
  )
})

test_that("printing", {
  skip_if_not_installed("RcppML")
  library(Matrix)
  rec <- recipe(mpg ~ ., mtcars) %>%
    step_nnmf_sparse(disp, drat)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
