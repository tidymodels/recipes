library(testthat)
library(recipes)

exp_dates <- data.frame(
  date = lubridate::ymd(c("2017-12-25", "2017-05-29", "2017-04-16")),
  holiday = c("ChristmasDay", "USMemorialDay", "Easter"),
  stringsAsFactors = FALSE
)
test_data <- data.frame(
  day = c(lubridate::ymd("2017-01-01") + lubridate::days(0:364), NA),
  stringsAsFactors = FALSE
)

is_equal_1 <- function(x) {
  x == 1 & !is.na(x)
}

test_that("Date class", {
  holiday_rec <- recipe(~day, test_data) |>
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
    lubridate::NA_Date_
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

test_that("Date class", {
  holiday_rec <- recipe(~day, test_data) |>
    step_holiday(
      all_predictors(),
      holidays = exp_dates$holiday,
      keep_original_cols = FALSE
    )

  holiday_rec <- prep(holiday_rec, training = test_data)
  holiday_ind <- bake(holiday_rec, test_data, all_predictors())

  expect_true(all(vapply(holiday_ind, is.integer, logical(1))))
})

test_that("works with no missing values - Date class", {
  test_data <- na.omit(test_data)

  holiday_rec <- recipe(~day, test_data) |>
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
  test_data$day <- lubridate::as_datetime(test_data$day, tz = "UTC")
  exp_dates$date <- lubridate::as_datetime(exp_dates$date, tz = "UTC")

  holiday_rec <- recipe(~day, test_data) |>
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
    as.POSIXct(NA, tz = "UTC")
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

test_that("Date class", {
  test_data$day <- as.POSIXct(test_data$day)
  exp_dates$date <- as.POSIXct(exp_dates$date)

  holiday_rec <- recipe(~day, test_data) |>
    step_holiday(
      all_predictors(),
      holidays = exp_dates$holiday,
      keep_original_cols = FALSE
    )

  holiday_rec <- prep(holiday_rec, training = test_data)
  holiday_ind <- bake(holiday_rec, test_data, all_predictors())

  expect_true(all(vapply(holiday_ind, is.integer, logical(1))))
})

test_that("works with no missing values - POSIXct class", {
  test_data <- na.omit(test_data)

  test_data$day <- as.POSIXct(test_data$day)
  exp_dates$date <- as.POSIXct(exp_dates$date)

  holiday_rec <- recipe(~day, test_data) |>
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

test_that("check_name() is used", {
  dat <- test_data
  dat$day_Easter <- dat$day

  rec <- recipe(~., dat) |>
    step_holiday(day, holidays = exp_dates$holiday)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("error on incorrect holidays argument", {
  expect_snapshot(
    error = TRUE,
    recipe(~., mtcars) |>
      step_holiday(holidays = c("Invalid Holiday", "NewYearsDay"))
  )
})

test_that("sparse = 'yes' works", {
  rec <- recipe(~., data = test_data)

  dense <- rec |>
    step_holiday(day, sparse = "no", keep_original_cols = FALSE) |>
    prep() |>
    bake(NULL)
  dense <- purrr::map(dense, as.integer) |> tibble::new_tibble()
  sparse <- rec |>
    step_holiday(day, sparse = "yes", keep_original_cols = FALSE) |>
    prep() |>
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_vector, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  rec <- recipe(~., data = test_data) |>
    step_holiday(day) |>
    prep()

  exp <- bake(rec, test_data)

  # Simulate old recipe
  rec$steps[[1]]$sparse <- NULL

  expect_identical(
    bake(rec, test_data),
    exp
  )
})

test_that(".recipes_toggle_sparse_args works", {
  rec <- recipe(~., data = test_data) |>
    step_holiday(day, sparse = "auto", keep_original_cols = TRUE)

  expect_equal(
    .recipes_estimate_sparsity(rec),
    364 / 365
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  holiday_rec <- recipe(~day, test_data) |>
    step_holiday(day, holidays = exp_dates$holiday) |>
    update_role(day, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  holiday_rec <- prep(holiday_rec, training = test_data)

  expect_snapshot(error = TRUE, bake(holiday_rec, exp_dates[, 2, drop = FALSE]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_holiday(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

test_that("keep_original_cols works", {
  new_names <- c("day_ChristmasDay", "day_USMemorialDay", "day_Easter")

  rec <- recipe(~day, test_data) |>
    step_holiday(
      all_predictors(),
      holidays = exp_dates$holiday,
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~day, test_data) |>
    step_holiday(
      all_predictors(),
      holidays = exp_dates$holiday,
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("day", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~day, test_data) |>
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = test_data)
  )
})

test_that("printing", {
  rec <- recipe(~day, test_data) |>
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- test_data
  rec <- recipe(~., data) |>
    step_holiday(day, holidays = exp_dates$holiday) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
