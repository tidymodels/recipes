library(testthat)
library(recipes)
library(lubridate)
library(tibble)


examples <- data.frame(
  times = ymd_hms("2022-05-06 10:01:07") +
  hours(1:5) + minutes(1:5) + seconds(1:5)
)

date_rec <- recipe(~ times, examples) %>%
  step_time(all_predictors())

feats <- c("ampm", "hour", "hour12", "minute", "second", "decimal_day")

test_that("default option", {
  # because of https://github.com/tidyverse/lubridate/issues/928
  skip_if(utils::packageVersion("lubridate") <= "1.7.9.9000")

  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), features = feats)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  date_exp <- tibble(
    times = examples$times,
    times_ampm = am(examples$times),
    times_hour = hour(examples$times),
    times_hour12 = as.integer(format(examples$times, "%I")),
    times_minute = minute(examples$times),
    times_second = second(examples$times),
    times_decimal_day = hour(examples$times) +
      (second(examples$times) + minute(examples$times) * 60) / 3600
  )
  expect_equal(date_res, date_exp)
})


test_that("nondefault options", {
  # because of https://github.com/tidyverse/lubridate/issues/928
  skip_if(utils::packageVersion("lubridate") <= "1.7.9.9000")

  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), features = c( "minute", "second"))

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  date_exp <- tibble(
    times = examples$times,
    times_minute = minute(examples$times),
    times_second = second(examples$times)
  )

  expect_equal(date_res, date_exp)
})

test_that("printing", {
  # because of https://github.com/tidyverse/lubridate/issues/928
  skip_if(utils::packageVersion("lubridate") <= "1.7.9.9000")

  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), features = feats)
  expect_snapshot(print(date_rec))
  expect_snapshot(prep(date_rec))
})

test_that("keep_original_cols works", {
  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), features = feats, keep_original_cols = FALSE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  expect_equal(
    colnames(date_res),
    paste0("times_", feats)
  )
})

test_that("can prep recipes with no keep_original_cols", {
  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), features = feats, keep_original_cols = FALSE)

  date_rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    date_rec <- prep(date_rec, training = examples, verbose = FALSE)
  )

  expect_error(
    date_res <- bake(date_rec, new_data = examples, all_predictors()),
    NA
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_time(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_time(rec)

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_time(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
