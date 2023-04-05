library(testthat)

test_that("default option", {
  examples <- data.frame(
    times = lubridate::ymd_hms("2022-05-06 10:01:07") +
      lubridate::hours(1:5) + lubridate::minutes(1:5) + lubridate::seconds(1:5)
  )

  feats <- c("am", "hour", "hour12", "minute", "second", "decimal_day")

  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), features = feats)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  date_exp <- tibble(
    times = examples$times,
    times_am = lubridate::am(examples$times),
    times_hour = lubridate::hour(examples$times),
    times_hour12 = as.integer(format(examples$times, "%I")),
    times_minute = lubridate::minute(examples$times),
    times_second = lubridate::second(examples$times),
    times_decimal_day = lubridate::hour(examples$times) +
      (lubridate::second(examples$times) +
       lubridate::minute(examples$times) * 60) / 3600
  )
  expect_equal(date_res, date_exp)
})

test_that("returns integers", {
  examples <- data.frame(
    times = lubridate::ymd_hms("2022-05-06 10:01:07") +
      lubridate::hours(1:5) + lubridate::minutes(1:5) + lubridate::seconds(1:5)
  )

  feats <- c("hour", "hour12", "minute")

  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), features = feats, keep_original_cols = FALSE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  expect_true(all(vapply(date_res, is.integer, logical(1))))
})


test_that("nondefault options", {
  examples <- data.frame(
    times = lubridate::ymd_hms("2022-05-06 10:01:07") +
      lubridate::hours(1:5) + lubridate::minutes(1:5) + lubridate::seconds(1:5)
  )

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

test_that("custom hour12 metric is correct", {
  full_day <- tibble(
    time = lubridate::ymd_hms("2000-01-01 00:00:00") +
      lubridate::seconds(seq(0, 60 * 60 * 24))
  )

  date_rec <- recipe(~ time, full_day) %>%
    step_time(all_predictors(), features = c( "hour12"))

  date_rec <- prep(date_rec, training = full_day)
  date_res <- bake(date_rec, new_data = full_day)

  hour12old <- function(x) {
    as.integer(format(x, "%I"))
  }

  date_exp <- tibble(
    time = full_day$time,
    time_hour12 = as.integer(format(full_day$time, "%I"))
  )

  expect_equal(date_res, date_exp)
})

test_that("check_name() is used", {
  dat <- tibble(
    time = lubridate::ymd_hms("2000-01-01 00:00:00") +
      lubridate::seconds(seq(0, 60 * 60 * 24))
  )
  dat$time_hour <- dat$time

  rec <- recipe(~ ., data = dat) %>%
    step_time(time)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("printing", {
  examples <- data.frame(
    times = lubridate::ymd_hms("2022-05-06 10:01:07") +
      lubridate::hours(1:5) + lubridate::minutes(1:5) + lubridate::seconds(1:5)
  )

  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors())
  expect_snapshot(print(date_rec))
  expect_snapshot(prep(date_rec))
})

test_that("keep_original_cols works", {
  examples <- data.frame(
    times = lubridate::ymd_hms("2022-05-06 10:01:07") +
      lubridate::hours(1:5) + lubridate::minutes(1:5) + lubridate::seconds(1:5)
  )

  date_rec <- recipe(~ times, examples) %>%
    step_time(all_predictors(), keep_original_cols = FALSE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  expect_equal(
    colnames(date_res),
    paste0("times_", c("hour", "minute", "second"))
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
