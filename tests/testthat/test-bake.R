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

test_that("bake() can stop after a given step number", {
  car_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors()) |>
    step_pca(all_predictors(), num_comp = 2) |>
    prep()

  first_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors()) |>
    prep()

  expect_identical(
    bake(car_rec, mtcars, stop_at = 1),
    bake(first_rec, mtcars)
  )
  expect_identical(
    bake(car_rec, mtcars, stop_at = 2),
    bake(car_rec, mtcars)
  )
})

test_that("bake() can stop after a given step id", {
  car_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors()) |>
    step_pca(all_predictors(), num_comp = 2) |>
    prep()

  expect_identical(
    bake(car_rec, mtcars, stop_at = car_rec$steps[[1]]$id),
    bake(car_rec, mtcars, stop_at = 1)
  )
})

test_that("bake() with stop_at works with selectors and composition", {
  car_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors()) |>
    step_pca(all_predictors(), num_comp = 2) |>
    prep()

  expect_named(
    bake(car_rec, mtcars, all_numeric_predictors(), stop_at = 2),
    c("PC1", "PC2")
  )
  expect_named(
    bake(car_rec, mtcars, starts_with("d"), stop_at = 1),
    c("disp", "drat")
  )
  expect_s4_class(
    bake(
      car_rec,
      mtcars,
      all_predictors(),
      stop_at = 1,
      composition = "dgCMatrix"
    ),
    "dgCMatrix"
  )
})

test_that("bake() with stop_at still skips steps with skip = TRUE", {
  car_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors()) |>
    step_filter(mpg > 0, skip = TRUE) |>
    step_scale(all_predictors()) |>
    prep()

  expect_identical(
    bake(car_rec, mtcars, stop_at = 2),
    bake(car_rec, mtcars, stop_at = 1)
  )
  expect_identical(
    bake(car_rec, mtcars, stop_at = 3),
    bake(car_rec, mtcars)
  )
})

test_that("bake() errors on bad stop_at", {
  car_rec <- recipe(cyl ~ ., mtcars) |>
    step_center(all_predictors(), id = "center") |>
    prep()

  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = 2))
  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = 0))
  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = 1.5))
  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = "nope"))
  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = TRUE))
  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = c(1, 1)))
  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = NA))
  expect_snapshot(error = TRUE, bake(car_rec, mtcars, stop_at = integer(0)))
  expect_snapshot(error = TRUE, bake(car_rec, new_data = NULL, stop_at = 1))
  expect_snapshot(
    error = TRUE,
    bake(prep(recipe(cyl ~ ., mtcars)), mtcars, stop_at = 1)
  )
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
