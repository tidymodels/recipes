library(recipes)
library(testthat)

test_that("order of columns after juice and bake", {
  car_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors()) |>
    step_scale(all_predictors())
  car_preped <- prep(car_rec, training = mtcars)
  expect_equal(
    colnames(juice(car_preped)),
    colnames(bake(car_preped, new_data = mtcars))
  )
})

test_that("can use tidyselect ops in bake() and juice() column selection", {
  car_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors())

  car_prepped <- prep(car_rec, training = mtcars)

  x <- bake(car_prepped, mtcars, where(is.numeric) & starts_with("c") & !cyl)
  y <- juice(car_prepped, where(is.numeric) & starts_with("c") & !cyl)

  expect_named(x, "carb")
  expect_named(y, "carb")
})

test_that("bake() and juice() doens't turn strings into factors #317", {
  exp_data <- tibble(f1 = factor(1), f2 = "1", c1 = "1")

  expect_identical(
    recipe(~., exp_data, strings_as_factors = TRUE) |>
      prep() |>
      juice(),
    tibble(f1 = factor(1), f2 = factor(1), c1 = factor(1))
  )

  # juice()
  expect_identical(
    recipe(~., exp_data, strings_as_factors = TRUE) |>
      step_factor2string(f1) |>
      prep() |>
      juice(),
    tibble(f1 = "1", f2 = factor(1), c1 = factor(1))
  )

  # bake(new_data = NULL)
  expect_identical(
    recipe(~., exp_data, strings_as_factors = TRUE) |>
      prep() |>
      bake(new_data = NULL),
    tibble(f1 = factor(1), f2 = factor(1), c1 = factor(1))
  )

  # bake(new_data = data)
  expect_identical(
    recipe(~., exp_data, strings_as_factors = TRUE) |>
      step_factor2string(f1) |>
      prep() |>
      bake(new_data = exp_data),
    tibble(f1 = "1", f2 = factor(1), c1 = factor(1))
  )
})
