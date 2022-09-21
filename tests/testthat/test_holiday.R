library(testthat)
library(recipes)
library(lubridate)

exp_dates <- data.frame(
  date = ymd(c("2017-12-25", "2017-05-29", "2017-04-16")),
  holiday = c("ChristmasDay", "USMemorialDay", "Easter"),
  stringsAsFactors = FALSE
)
test_data <- data.frame(
  day = c(ymd("2017-01-01") + days(0:364), NA),
  stringsAsFactors = FALSE
)

is_equal_1 <- function(x) {
  x == 1 & !is.na(x)
}

test_that("Date class", {
  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  holiday_rec <- prep(holiday_rec, training = test_data)
  holiday_ind <- bake(holiday_rec, test_data)

  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_USMemorialDay)],
    exp_dates$date[exp_dates$holiday == "USMemorialDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_ChristmasDay)],
    exp_dates$date[exp_dates$holiday == "ChristmasDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_Easter)],
    exp_dates$date[exp_dates$holiday == "Easter"]
  )
  expect_equal(
    holiday_ind$day[is.na(test_data$day)],
    NA_Date_
  )
  expect_equal(
    holiday_ind$day_ChristmasDay[is.na(test_data$day)],
    NA_integer_
  )
  expect_equal(
    holiday_ind$day_USMemorialDay[is.na(test_data$day)],
    NA_integer_
  )
  expect_equal(
    holiday_ind$day_Easter[is.na(test_data$day)],
    NA_integer_
  )
})

test_that("works with no missing values - Date class", {
  test_data <- na.omit(test_data)

  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  holiday_rec <- prep(holiday_rec, training = test_data)
  holiday_ind <- bake(holiday_rec, test_data)

  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_USMemorialDay)],
    exp_dates$date[exp_dates$holiday == "USMemorialDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_ChristmasDay)],
    exp_dates$date[exp_dates$holiday == "ChristmasDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_Easter)],
    exp_dates$date[exp_dates$holiday == "Easter"]
  )
})

test_that("POSIXct class", {
  test_data$day <- as.POSIXct(test_data$day)
  exp_dates$date <- as.POSIXct(exp_dates$date)

  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  holiday_rec <- prep(holiday_rec, training = test_data)
  holiday_ind <- bake(holiday_rec, test_data)

  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_USMemorialDay)],
    exp_dates$date[exp_dates$holiday == "USMemorialDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_ChristmasDay)],
    exp_dates$date[exp_dates$holiday == "ChristmasDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_Easter)],
    exp_dates$date[exp_dates$holiday == "Easter"]
  )
  expect_equal(
    holiday_ind$day[is.na(test_data$day)],
    as.POSIXct(NA, tz = NULL)
  )
  expect_equal(
    holiday_ind$day_ChristmasDay[is.na(test_data$day)],
    NA_integer_
  )
  expect_equal(
    holiday_ind$day_USMemorialDay[is.na(test_data$day)],
    NA_integer_
  )
  expect_equal(
    holiday_ind$day_Easter[is.na(test_data$day)],
    NA_integer_
  )
})

test_that("works with no missing values - POSIXct class", {
  test_data <- na.omit(test_data)

  test_data$day <- as.POSIXct(test_data$day)
  exp_dates$date <- as.POSIXct(exp_dates$date)

  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  holiday_rec <- prep(holiday_rec, training = test_data)
  holiday_ind <- bake(holiday_rec, test_data)

  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_USMemorialDay)],
    exp_dates$date[exp_dates$holiday == "USMemorialDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_ChristmasDay)],
    exp_dates$date[exp_dates$holiday == "ChristmasDay"]
  )
  expect_equal(
    holiday_ind$day[is_equal_1(holiday_ind$day_Easter)],
    exp_dates$date[exp_dates$holiday == "Easter"]
  )
})

test_that("printing", {
  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)
  expect_snapshot(print(holiday_rec))
  expect_snapshot(prep(holiday_rec))
})

test_that("keep_original_cols works", {
  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(all_predictors(),
      holidays = exp_dates$holiday,
      keep_original_cols = FALSE
    )

  holiday_rec <- prep(holiday_rec, training = test_data)
  holiday_ind <- bake(holiday_rec, test_data)

  expect_equal(
    colnames(holiday_ind),
    c(
      "day_ChristmasDay",
      "day_USMemorialDay",
      "day_Easter"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  holiday_rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    holiday_rec <- prep(holiday_rec, training = test_data, verbose = FALSE)
  )

  expect_error(
    holiday_ind <- bake(holiday_rec, new_data = test_data, all_predictors()),
    NA
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_holiday(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_holiday(rec)

  expect <- tibble(terms = character(), holiday = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_holiday(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  holiday_rec <- recipe(~day, test_data) %>%
    step_holiday(day, holidays = exp_dates$holiday) %>%
    update_role(day, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  holiday_rec <- prep(holiday_rec, training = test_data)

  expect_error(bake(holiday_rec, exp_dates[, 2, drop = FALSE]),
               class = "new_data_missing_column")
})
