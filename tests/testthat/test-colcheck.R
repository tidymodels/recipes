library(testthat)
library(recipes)

rp1 <- recipe(mtcars, cyl ~ .)
rp2 <- recipe(mtcars, cyl ~ mpg + drat)

test_that("check_col works in the prep stage", {
  expect_no_error(rp1 |> check_cols(all_predictors()) |> prep())
  expect_no_error(rp2 |> check_cols(all_predictors()) |> prep())
  expect_no_error(rp2 |> check_cols(cyl, mpg, drat) |> prep())
  expect_no_error(rp2 |> check_cols(cyl, mpg) |> prep())
})

test_that("check_col works in the bake stage", {
  expect_no_error(
    rp1 |> check_cols(all_predictors()) |> prep() |> bake(mtcars)
  )
  expect_equal(
    rp1 |> check_cols(all_predictors()) |> prep() |> bake(mtcars),
    tibble(mtcars[, c(1, 3:11, 2)])
  )

  expect_no_error(
    rp2 |> check_cols(cyl, mpg, drat) |> prep() |> bake(mtcars)
  )
  expect_equal(
    rp2 |> check_cols(cyl, mpg, drat) |> prep() |> bake(mtcars),
    tibble(mtcars[, c(1, 5, 2)])
  )

  expect_no_error(
    rp1 |> check_cols(all_predictors()) |> prep() |> bake(mtcars)
  )
  expect_equal(
    rp1 |> check_cols(all_predictors()) |> prep() |> bake(mtcars),
    tibble(mtcars[, c(1, 3:11, 2)])
  )
  expect_no_error(
    rp2 |> check_cols(cyl, mpg, drat) |> prep() |> bake(mtcars)
  )
  expect_equal(
    rp2 |> check_cols(cyl, mpg, drat) |> prep() |> bake(mtcars),
    tibble(mtcars[, c(1, 5, 2)])
  )
  expect_snapshot(
    error = TRUE,
    rp1 |> check_cols(all_predictors()) |> prep() |> bake(mtcars[-1])
  )
  expect_snapshot(
    error = TRUE,
    rp2 |> check_cols(cyl, mpg, drat) |> prep() |> bake(mtcars[, c(2, 5)])
  )
})

test_that("non-standard roles during bake/predict", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if(packageVersion("workflows") < "0.2.6.9001")
  skip_if(packageVersion("parsnip") < "1.0.0")

  # ----------------------------------------------------------------------------

  library(workflows)
  library(parsnip)

  # ----------------------------------------------------------------------------

  data(Chicago, package = "modeldata", envir = current_env())

  Chicago <- Chicago |> dplyr::select(ridership, date, Austin, Belmont)

  set.seed(1)
  Chicago$wts <- importance_weights(runif(nrow(Chicago)))

  # ----------------------------------------------------------------------------

  base_wflow <-
    workflow() |>
    add_model(linear_reg())

  # ----------------------------------------------------------------------------
  # non-standard role used in a step

  ## no case weights, default blueprint
  role_rec <-
    recipe(ridership ~ date + Austin + Belmont, data = Chicago) |>
    update_role(date, new_role = "date") |>
    step_date(date)

  role_wflow <-
    base_wflow |>
    add_recipe(role_rec)

  role_fit <- fit(role_wflow, data = Chicago)

  expect_no_error(predict(role_fit, head(Chicago)))

  # This should require 'date' to predict.
  # The error comes from hardhat, so we don't snapshot it because we don't own it.
  expect_error(
    predict(role_fit, Chicago |> select(-date))
  )

  # ----------------------------------------------------------------------------
  # non-standard role used in a step and case weights

  role_wts_rec <-
    recipe(ridership ~ ., data = Chicago) |>
    update_role(date, new_role = "date") |>
    step_date(date)

  role_wts_wflow <-
    base_wflow |>
    add_recipe(role_wts_rec) |>
    add_case_weights(wts)

  role_wts_fit <- fit(role_wts_wflow, data = Chicago)

  # This should require 'date' but not 'wts' to predict
  expect_no_error(predict(role_wts_fit, head(Chicago)))
  expect_no_error(predict(role_wts_fit, head(Chicago) |> select(-wts)))
  expect_snapshot(
    error = TRUE,
    predict(role_wts_fit, head(Chicago) |> select(-date))
  )

  # ----------------------------------------------------------------------------
  # Removing variable after use

  rm_rec <-
    recipe(ridership ~ date + Austin + Belmont, data = Chicago) |>
    step_date(date, keep_original_cols = FALSE)

  rm_wflow <-
    base_wflow |>
    add_recipe(rm_rec)

  rm_fit <- fit(rm_wflow, data = Chicago)

  # This should require 'date' to predict
  expect_snapshot(
    error = TRUE,
    predict(rm_fit, Chicago |> select(-date))
  )

  # ----------------------------------------------------------------------------
  # Removing variable after use, with case weights

  rm_wts_rec <-
    recipe(ridership ~ ., data = Chicago) |>
    step_date(date, keep_original_cols = FALSE)

  rm_wts_wflow <-
    base_wflow |>
    add_recipe(rm_wts_rec) |>
    add_case_weights(wts)

  rm_wts_fit <- fit(rm_wts_wflow, data = Chicago)

  # This should require 'date' but not 'wts' to predict
  expect_no_error(predict(rm_fit, Chicago |> select(-wts)))
  expect_snapshot(
    error = TRUE,
    predict(rm_fit, Chicago |> select(-date))
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # the main check_cols() purpose of this function is to test for this
  # event
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_cols(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_cols(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_cols(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(mpg ~ ., mtcars) |>
    check_cols(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    check_cols(all_numeric_predictors()) |>
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
