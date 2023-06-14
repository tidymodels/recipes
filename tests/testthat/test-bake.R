library(recipes)
library(testthat)

test_that("order of columns after juice and bake", {
  car_rec <- recipe(cyl ~ ., mtcars) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())
  car_preped <- prep(car_rec, training = mtcars)
  expect_equal(
    colnames(juice(car_preped)),
    colnames(bake(car_preped, new_data = mtcars))
  )
})

test_that("can use tidyselect ops in bake() and juice() column selection", {
  car_rec <- recipe(cyl ~ ., mtcars) %>%
    step_center(all_predictors())

  car_prepped <- prep(car_rec, training = mtcars)

  x <- bake(car_prepped, mtcars, where(is.numeric) & starts_with("c") & !cyl)
  y <- juice(car_prepped, where(is.numeric) & starts_with("c") & !cyl)

  expect_named(x, "carb")
  expect_named(y, "carb")
})
