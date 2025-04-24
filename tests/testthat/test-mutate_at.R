library(testthat)
library(recipes)

iris_rec <- recipe(~., data = iris)

test_that("basic usage", {
  rec <-
    iris_rec |>
    step_mutate_at(contains("Length"), fn = log)

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    mutate(
      Sepal.Length = log(Sepal.Length),
      Petal.Length = log(Petal.Length)
    )

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150) |>
    mutate(
      Sepal.Length = log(Sepal.Length),
      Petal.Length = log(Petal.Length)
    )
  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("mulitple functions", {
  rec <-
    iris_rec |>
    step_mutate_at(contains("Length"), fn = list(a = log, b = sqrt))

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    mutate(
      Sepal.Length_a = log(Sepal.Length),
      Petal.Length_a = log(Petal.Length),
      Sepal.Length_b = sqrt(Sepal.Length),
      Petal.Length_b = sqrt(Petal.Length)
    )

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150) |>
    mutate(
      Sepal.Length_a = log(Sepal.Length),
      Petal.Length_a = log(Petal.Length),
      Sepal.Length_b = sqrt(Sepal.Length),
      Petal.Length_b = sqrt(Petal.Length)
    )
  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("no input", {
  expect_snapshot(
    error = TRUE,
    iris_rec |>
      step_mutate_at() |>
      prep(training = iris) |>
      bake(new_data = NULL, composition = "data.frame")
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_mutate_at() is one of the thin wrappers around dplyr functions and
  # is thus hard to check against
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_mutate_at(rec, fn = mean)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_mutate_at(rec1, fn = mean)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_mutate_at(rec, fn = mean)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = iris) |>
    step_mutate_at(contains("Sepal"), fn = log)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_mutate_at(all_numeric_predictors(), fn = abs) |>
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
