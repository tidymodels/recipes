library(testthat)
library(recipes)

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

test_that("detect_step detects step_naomit", {
  # presumably at some point step_naomit will become something other than
  # a step. this will probably make `step_naomit` undetectable by `detect_step`.
  # this test is here to make sure that doesn't happen silently

  rec <- recipe(Species ~ ., data = iris) %>%
    step_naomit(all_predictors())

  expect_true(detect_step(rec, "step_naomit"))
})

test_that("something prints", {
  rec <- recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(all_predictors())

  expect_output(print(rec))
  expect_output(prep(rec, training = airquality, verbose = TRUE))
})

