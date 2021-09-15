library(dplyr)
library(testthat)
library(recipes)

n <- 10
start <- as.Date('1999/01/01')
end <- as.Date('2000/01/01')

set.seed(1)

test_that("default lag works on a single feature",  {

  set.seed(27)
  df <- tibble(x = rnorm(n), t = sample(seq(start, end, by = "day"), n))

  # lags numeric data
  baked <- recipe(~ ., data = df) %>%
    step_lag(t, lag = 2) %>%
    prep(df) %>%
    bake(df)
  expected <- mutate(df, lag_2_t = dplyr::lag(t, 2))
  expect_equal(baked, expected)

  # lags date data
  baked <- recipe(~ ., data = df) %>%
    step_lag(t, lag = 2) %>%
    prep(df) %>%
    bake(df)
  expected <- mutate(df, lag_2_t = dplyr::lag(t, 2))
  expect_equal(baked, expected)

  # default argument works as expect
  baked <- recipe(~ ., data = df) %>%
    step_lag(t, lag = 2, default = start) %>%
    prep(df) %>%
    bake(df)
  expected <- df
  expected$lag_2_t <- dplyr::lag(expected$t, 2, default = start)
  expect_equal(baked, expected)

  # errors out on non-integer lag
  expect_error({
    prepped_rec <- recipe(~ ., data = df) %>%
      step_lag(x, lag = 0.5) %>%
      prep(df)
  })
})

test_that("specification of multiple lags in a vector",  {

  set.seed(29)
  df <- tibble(t = sample(seq(start, end, by = "day"), n),
               tt = sample(seq(start, end, by = "day"), n))

  baked <- recipe(~ ., data = df) %>%
    step_lag(t, tt, lag = c(1, 2)) %>%
    prep(df) %>%
    bake(df)

  expected <- df %>%
    mutate(lag_1_t = dplyr::lag(t, 1),
           lag_1_tt = dplyr::lag(tt, 1),
           lag_2_t = dplyr::lag(t, 2),
           lag_2_tt = dplyr::lag(tt, 2))

  expect_equal(baked, expected)
})

test_that('something prints', {
  df <- tibble(x = rnorm(n), t = sample(seq(start, end, by = "day"), n))

  rec <- recipe(~ ., data = df) %>%
    step_lag(t)

  expect_output(print(rec))
  expect_output(prep(rec, training = df, verbose = TRUE))
})

rm(n, start, end)
