library(dplyr)
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
  baked <- recipe(~., data = df) %>%
    step_lag(t, lag = 2) %>%
    prep(df) %>%
    bake(df)
  expected <- mutate(df, lag_2_t = dplyr::lag(t, 2))
  expect_equal(baked, expected)

  # lags date data
  baked <- recipe(~., data = df) %>%
    step_lag(t, lag = 2) %>%
    prep(df) %>%
    bake(df)
  expected <- mutate(df, lag_2_t = dplyr::lag(t, 2))
  expect_equal(baked, expected)

  # default argument works as expect
  baked <- recipe(~., data = df) %>%
    step_lag(t, lag = 2, default = start) %>%
    prep(df) %>%
    bake(df)
  expected <- df
  expected$lag_2_t <- dplyr::lag(expected$t, 2, default = start)
  expect_equal(baked, expected)

  # errors out on non-integer lag
  expect_snapshot(error = TRUE,
    prepped_rec <- recipe(~., data = df) %>%
      step_lag(x, lag = 0.5) %>%
      prep(df)
  )
})

test_that("specification of multiple lags in a vector", {
  set.seed(29)
  df <- tibble(
    t = sample(seq(start, end, by = "day"), n),
    tt = sample(seq(start, end, by = "day"), n)
  )

  baked <- recipe(~., data = df) %>%
    step_lag(t, tt, lag = c(1, 2)) %>%
    prep(df) %>%
    bake(df)

  expected <- df %>%
    mutate(
      lag_1_t = dplyr::lag(t, 1),
      lag_2_t = dplyr::lag(t, 2),
      lag_1_tt = dplyr::lag(tt, 1),
      lag_2_tt = dplyr::lag(tt, 2)
    )

  expect_equal(baked, expected)
})

test_that("something prints", {
  df <- tibble(x = rnorm(n), t = sample(seq(start, end, by = "day"), n))

  rec <- recipe(~., data = df) %>%
    step_lag(t)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

rm(n, start, end)


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
  # TODO: Uncomment if we get a tidy method for step_lag()
  expect_true(TRUE)

  # rec <- recipe(mpg ~ ., mtcars)
  # rec <- step_lag(rec)
  #
  # expect <- tibble(terms = character(), id = character())
  #
  # expect_identical(tidy(rec, number = 1), expect)
  #
  # rec <- prep(rec, mtcars)
  #
  # expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lag(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  set.seed(27)

  n <- 10
  start <- as.Date("1999/01/01")
  end <- as.Date("2000/01/01")

  df <- tibble(x = rnorm(n), t = sample(seq(start, end, by = "day"), n))

  # lags numeric data
  rec <- recipe(~., data = df) %>%
    step_lag(t, lag = 2) %>%
    update_role(t, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)%>%
    prep(df)

  expect_error(bake(rec, new_data = df[, 1, drop = FALSE]),
               class = "new_data_missing_column")
})
