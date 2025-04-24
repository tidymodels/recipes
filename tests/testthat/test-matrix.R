library(testthat)
library(recipes)

###################################################################

skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")

Sacramento$city <- as.factor(Sacramento$city)
Sacramento$beds <- as.factor(Sacramento$beds)
Sacramento$zip <- as.factor(Sacramento$zip)

sacr_tr <- Sacramento[1:400, ]
sacr_te <- Sacramento[(401:800), ]

###################################################################

rec <- recipe(~., data = sacr_tr) |>
  step_impute_mode(all_nominal()) |>
  step_impute_mean(all_numeric()) |>
  step_dummy(zip, city) |>
  prep(training = sacr_tr)

###################################################################

test_that("correct types", {
  bake_default <- bake(rec, new_data = sacr_te, all_numeric())
  bake_sparse <-
    bake(rec, new_data = sacr_te, all_numeric(), composition = "matrix")
  bake_sparse_1d <-
    bake(rec, new_data = sacr_te, sqft, composition = "matrix")
  juice_default <- juice(rec, all_numeric())
  juice_sparse <-
    juice(rec, all_numeric(), composition = "matrix")
  juice_sparse_1d <-
    juice(rec, sqft, composition = "matrix")

  expect_equal(class(bake_default), class(tibble()))
  expect_equal(class(juice_default), class(tibble()))

  expect_true(inherits(bake_sparse, "matrix"))
  expect_true(inherits(juice_sparse, "matrix"))

  expect_true(inherits(bake_sparse_1d, "matrix"))
  expect_true(inherits(juice_sparse_1d, "matrix"))

  expect_equal(
    hardhat::recompose(bake_default, composition = "matrix"),
    bake_sparse
  )
  expect_equal(
    hardhat::recompose(juice_default, composition = "matrix"),
    juice_sparse
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    bake(rec, new_data = sacr_te, composition = "matrix")
  )
  expect_snapshot(error = TRUE, juice(rec, composition = "matrix"))
})
