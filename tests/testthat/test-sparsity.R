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
    bake(rec, new_data = sacr_te, all_numeric(), composition = "dgCMatrix")
  bake_sparse_1d <-
    bake(rec, new_data = sacr_te, sqft, composition = "dgCMatrix")
  juice_default <- juice(rec, all_numeric())
  juice_sparse <-
    juice(rec, all_numeric(), composition = "dgCMatrix")
  juice_sparse_1d <-
    juice(rec, sqft, composition = "dgCMatrix")

  expect_equal(class(bake_default), class(tibble()))
  expect_equal(class(juice_default), class(tibble()))

  expect_equal(as.vector(class(bake_sparse)), "dgCMatrix")
  expect_equal(as.vector(class(juice_sparse)), "dgCMatrix")

  expect_equal(as.vector(class(bake_sparse_1d)), "dgCMatrix")
  expect_equal(as.vector(class(juice_sparse_1d)), "dgCMatrix")

  expect_equal(
    hardhat::recompose(bake_default, composition = "dgCMatrix"),
    bake_sparse
  )
  expect_equal(
    hardhat::recompose(juice_default, composition = "dgCMatrix"),
    juice_sparse
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    bake(rec, new_data = sacr_te, composition = "dgCMatrix")
  )
  expect_snapshot(error = TRUE, juice(rec, composition = "dgCMatrix"))

  data("ames", package = "modeldata")
  expect_snapshot(
    error = TRUE,
    recipe(~., data = ames) |>
      prep() |>
      bake(new_data = NULL, composition = "dgCMatrix")
  )
})

test_that("issue 206 - NA values are kept when requesting matrix composition", {
  df <- data.frame(x = factor(c("x", "x", "y")), x2 = c(NA, 1, NA))

  rec <- recipe(x2 ~ ., data = df) |>
    step_dummy(x) |>
    prep(df)

  res_mat <- bake(rec, df, composition = "matrix")
  res_sparse <- bake(rec, df, composition = "dgCMatrix")

  expect_equal(nrow(res_mat), 3)
  expect_equal(nrow(res_sparse), 3)

  expect_equal(as.vector(res_mat[, "x2"]), df$x2)
  expect_equal(as.vector(res_sparse[, "x2"]), df$x2)
})
