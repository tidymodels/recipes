library(recipes)
library(testthat)

test_that("order of columns after juice and bake",{
  car_rec <- recipe(cyl ~ ., mtcars) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())
  car_preped <- prep(car_rec, training = mtcars)
  expect_equal(colnames(juice(car_preped)), colnames(bake(car_preped, new_data = mtcars)))
})
