library(testthat)
library(recipes)

# ------------------------------------------------------------------------------

iris_rec <- recipe(~., data = iris)

# ------------------------------------------------------------------------------

test_that("basic usage", {
  rec <-
    iris_rec |>
    step_slice(1:5)

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    slice(1:5)

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150)
  dplyr_test <- dplyr_test[, names(rec_train)]
  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("skip = FALSE", {
  rec <-
    iris_rec |>
    step_slice(1:5, skip = FALSE)

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    slice(1:5)

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150) |>
    slice(1:5)
  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  values <- 1:5
  rec_1 <-
    iris_rec |>
    step_slice(values)

  prepped_1 <- prep(rec_1, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    slice(values)

  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)

  expect_no_error(
    rec_2 <-
      iris_rec |>
      step_slice(!!values)
  )

  prepped_2 <- prep(rec_2, training = iris |> slice(1:75))

  rm(values)
  expect_snapshot(error = TRUE, prep(rec_1, training = iris |> slice(1:75)))
  expect_no_error(
    prepped_2 <- prep(rec_2, training = iris |> slice(1:75))
  )
  rec_2_train <- bake(prepped_2, new_data = NULL)
  expect_equal(dplyr_train, rec_2_train)
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)

  rec <- recipe(~., mtcars) |>
    step_slice(1:10) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$vs))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$am))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_slice() is special as it doesn't care about the incoming data
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_slice(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_slice(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_slice(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = iris) |>
    step_slice(1:2)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_slice(1:10) |>
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
