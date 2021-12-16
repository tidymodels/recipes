library(recipes)
library(testthat)

test_that("order of columns after juice and bake",{
  car_rec <- recipe(cyl ~ ., mtcars) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())
  car_preped <- prep(car_rec, training = mtcars)
  expect_equal(colnames(juice(car_preped)), colnames(bake(car_preped, new_data = mtcars)))
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

test_that("bake errors if predictor cols are missing from new_data", {

  error1 <- "The following cols are missing from `new_data`: mpg"
  error2 <- "The following cols are missing from `new_data`: cyl"
  error26 <- "The following cols are missing from `new_data`: cyl, disp, hp, drat, wt"
  rec1 <- recipe(mpg ~ ., data = mtcars)

  expect_error(rec1 %>% prep() %>% bake(mtcars), NA)
  expect_error(rec1 %>% prep() %>% bake(mtcars[-1]), NA)
  expect_error(rec1 %>% prep() %>% bake(mtcars[-2]), error2)
  expect_error(rec1 %>% prep() %>% bake(mtcars[-(2:6)]), error26)

  mtcars_mat <- as.matrix(mtcars)
  rec2 <- recipe(mpg ~ ., data = mtcars_mat)

  expect_error(rec2 %>% prep() %>% bake(mtcars_mat), NA)
  expect_error(rec2 %>% prep() %>% bake(mtcars_mat[, -1]), NA)
  expect_error(rec2 %>% prep() %>% bake(mtcars_mat[, -2]), error2)
  expect_error(rec2 %>% prep() %>% bake(mtcars_mat[, -(2:6)]), error26)

  mtcars_mat <- as.matrix(mtcars)
  rec3 <- recipe(x = mtcars_mat)

  expect_error(rec3 %>% prep() %>% bake(mtcars_mat), NA)
  expect_error(rec3 %>% prep() %>% bake(mtcars_mat[, -1]), error1)
  expect_error(rec3 %>% prep() %>% bake(mtcars_mat[, -2]), error2)
  expect_error(rec3 %>% prep() %>% bake(mtcars_mat[, -(2:6)]), error26)
})
