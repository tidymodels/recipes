library(testthat)
library(recipes)
library(tidyr)

context("step_naomit")

test_that("step_naomit on all columns", {

  baked <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(everything()) %>%
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

  expect_equal(baked, na.omit(airquality))
})

test_that("step_naomit on subset of columns", {

  baked <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(Ozone, Solar.R) %>%
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

  expect_equal(baked, tidyr::drop_na(airquality, Ozone, Solar.R))

  baked2 <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(Solar.R) %>%
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

  expect_equal(baked2, tidyr::drop_na(airquality, Solar.R))
})

test_that("something prints", {
  rec <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(all_predictors())

  expect_output(print(rec))
  expect_output(prep(rec, training = airquality, verbose = TRUE))
})

