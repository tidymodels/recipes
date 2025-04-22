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
  bake_df <-
    bake(rec, new_data = sacr_te, all_numeric(), composition = "data.frame")
  bake_df_1d <-
    bake(rec, new_data = sacr_te, sqft, composition = "data.frame")
  juice_default <- juice(rec, all_numeric())
  juice_df <-
    juice(rec, all_numeric(), composition = "data.frame")
  juice_df_1d <-
    juice(rec, sqft, composition = "data.frame")

  expect_equal(class(bake_default), class(tibble()))
  expect_equal(class(juice_default), class(tibble()))

  expect_equal(as.vector(class(bake_df)), "data.frame")
  expect_equal(as.vector(class(juice_df)), "data.frame")

  expect_equal(as.vector(class(bake_df_1d)), "data.frame")
  expect_equal(as.vector(class(juice_df_1d)), "data.frame")
})
