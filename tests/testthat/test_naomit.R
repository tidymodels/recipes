library(testthat)
library(recipes)
library(tidyr)

context("step_naomit")

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

