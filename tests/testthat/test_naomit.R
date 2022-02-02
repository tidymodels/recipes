library(testthat)
library(recipes)
library(tidyr)

test_that("step_naomit on all columns", {

  baked <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(everything()) %>%
    prep(airquality, verbose = FALSE) %>%
    juice()

  na_res <- tibble(na.omit(airquality))
  attributes(na_res)$na.action <- NULL

  expect_equal(baked, na_res[, c(2:6, 1)])
})

test_that("step_naomit on subset of columns", {

  baked <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(Ozone, Solar.R) %>%
    prep(airquality, verbose = FALSE) %>%
    juice()

  na_res <- tibble(tidyr::drop_na(airquality, Ozone, Solar.R))

  expect_equal(baked, na_res[, c(2:6, 1)])

  baked2 <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(Solar.R) %>%
    prep(airquality, verbose = FALSE) %>%
    juice()

  na_res2 <- tibble(tidyr::drop_na(airquality, Solar.R))

  expect_equal(baked2, na_res2[, c(2:6, 1)])
})

test_that("something prints", {
  rec <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(all_predictors())

  expect_output(print(rec))
  expect_output(prep(rec, training = airquality, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_naomit(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_naomit(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_naomit(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

