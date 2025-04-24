library(testthat)
library(recipes)

n <- 10
start <- as.Date("1999/01/01")
end <- as.Date("2000/01/01")

set.seed(1)

test_that("default lag works on a single feature", {
  set.seed(27)
  df <- tibble(x = rnorm(n), t = sample(seq(start, end, by = "day"), n))

  # lags numeric data
  baked <- recipe(~., data = df) |>
    step_lag(t, lag = 2) |>
    prep(df) |>
    bake(df)
  expected <- mutate(df, lag_2_t = dplyr::lag(t, 2))
  expect_equal(baked, expected)

  # lags date data
  baked <- recipe(~., data = df) |>
    step_lag(t, lag = 2) |>
    prep(df) |>
    bake(df)
  expected <- mutate(df, lag_2_t = dplyr::lag(t, 2))
  expect_equal(baked, expected)

  # default argument works as expect
  baked <- recipe(~., data = df) |>
    step_lag(t, lag = 2, default = start) |>
    prep(df) |>
    bake(df)
  expected <- df
  expected$lag_2_t <- dplyr::lag(expected$t, 2, default = start)
  expect_equal(baked, expected)

  # errors out on non-integer lag
  expect_snapshot(
    error = TRUE,
    prepped_rec <- recipe(~., data = df) |>
      step_lag(x, lag = 0.5) |>
      prep(df)
  )

  expect_snapshot(
    recipe(~., data = df) |>
      step_lag(x, prefix = 2) |>
      prep(),
    error = TRUE
  )
})

test_that("specification of multiple lags in a vector", {
  set.seed(29)
  df <- tibble(
    t = sample(seq(start, end, by = "day"), n),
    tt = sample(seq(start, end, by = "day"), n)
  )

  baked <- recipe(~., data = df) |>
    step_lag(t, tt, lag = c(1, 2)) |>
    prep(df) |>
    bake(df)

  expected <- df |>
    mutate(
      lag_1_t = dplyr::lag(t, 1),
      lag_2_t = dplyr::lag(t, 2),
      lag_1_tt = dplyr::lag(tt, 1),
      lag_2_tt = dplyr::lag(tt, 2)
    )

  expect_equal(baked, expected)
})

rm(n, start, end)

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_double(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_double(mtcars$am)
  rec <- recipe(~ am + vs, data = mtcars) |>
    step_lag(am, vs)

  rec_trained <- prep(rec, training = mtcars, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = mtcars)

  expect_true(all(vapply(rec_trans, sparsevctrs::is_sparse_double, logical(1))))

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  set.seed(27)

  n <- 10
  start <- as.Date("1999/01/01")
  end <- as.Date("2000/01/01")

  df <- tibble(x = rnorm(n), t = sample(seq(start, end, by = "day"), n))

  # lags numeric data
  rec <- recipe(~., data = df) |>
    step_lag(t, lag = 2) |>
    update_role(t, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep(df)

  expect_snapshot(error = TRUE, bake(rec, new_data = df[, 1, drop = FALSE]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lag(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_lag(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lag(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("lag_1_mpg")

  rec <- recipe(~mpg, mtcars) |>
    step_lag(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~mpg, mtcars) |>
    step_lag(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("mpg", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~mpg, mtcars) |>
    step_lag(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = mtcars)
  )
})

test_that("printing", {
  rec <- recipe(~., data = mtcars) |>
    step_lag(disp)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_lag(all_numeric_predictors()) |>
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
