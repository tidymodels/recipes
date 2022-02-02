library(testthat)
library(recipes)
library(tidyr)

train <-
  tibble::tibble(
    col1 = c("a", "b", "c"),
    col2 = c(NA, "d", "e"),
    col3 = c("k", NA, "g")
  )

test <-
  tibble::tibble(
    col1 = c(NA, NA, NA),
    col2 = c("t", "d", "e"),
    col3 = c("z", "f", NA)
  )

test_that("step_indicate_na populates binaries correctly", {

  rec1 <- recipe(train) %>%
    step_indicate_na(col1) %>%
    prep(train, verbose = FALSE, retain = TRUE)

  expect_equal(
    bake(rec1, train)$na_ind_col1,
    c(0, 0, 0)
  )

  expect_equal(
    bake(rec1, test)$na_ind_col1,
    c(1, 1, 1)
  )

  rec2 <- recipe(train) %>%
    step_indicate_na(col2, col3) %>%
    prep(train, verbose = FALSE, retain = TRUE)

  expect_equal(bake(rec2, train)$na_ind_col2, c(1, 0, 0))
  expect_equal(bake(rec2, train)$na_ind_col3, c(0, 1, 0))

  expect_equal(bake(rec2, test)$na_ind_col2, c(0, 0, 0))
  expect_equal(bake(rec2, test)$na_ind_col3, c(0, 0, 1))

})

test_that("step_indicate_na on all columns", {

  baked <- recipe(Ozone ~ ., data = airquality) %>%
    step_indicate_na(everything()) %>%
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

  expect_named(
    baked,
    c("Solar.R", "Wind", "Temp", "Month", "Day", "Ozone",
      "na_ind_Solar.R", "na_ind_Wind", "na_ind_Temp",
      "na_ind_Month", "na_ind_Day", "na_ind_Ozone"
      )
    )
})

test_that("step_indicate_na on subset of columns", {

  baked <- recipe(Ozone ~ ., data = airquality) %>%
    step_indicate_na(Ozone, Solar.R) %>%
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

  expect_named(
    baked,
    c("Solar.R", "Wind", "Temp", "Month", "Day",
      "Ozone", "na_ind_Ozone", "na_ind_Solar.R"
      )
    )

  baked2 <- recipe(Ozone ~ ., data = airquality) %>%
    step_indicate_na(Solar.R) %>%
    prep(airquality, verbose = FALSE, retain = TRUE) %>%
    juice()

  expect_named(
    baked2,
    c("Solar.R", "Wind", "Temp", "Month", "Day",
      "Ozone", "na_ind_Solar.R"
      )
    )
})

test_that("something prints", {
  rec <- recipe(Ozone ~ ., data = airquality) %>%
    step_indicate_na(all_predictors())

  expect_output(print(rec))
  expect_output(prep(rec, training = airquality, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_indicate_na(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_indicate_na(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_indicate_na(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
