library(testthat)
library(recipes)

# ------------------------------------------------------------------------------

iris_rec <- recipe(~., data = iris)

# ------------------------------------------------------------------------------

test_that("basic usage", {
  rec <-
    iris_rec |>
    step_arrange(desc(Sepal.Length), 1 / Petal.Length)

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150) |>
    dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)
  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  sort_vars <- c("Sepal.Length", "Petal.Length")
  sort_vars <- syms(sort_vars)
  rec_1 <-
    iris_rec |>
    step_arrange(!!!sort_vars)

  prepped_1 <- prep(rec_1, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    arrange(Sepal.Length, Petal.Length)

  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)
})

test_that("no input", {
  no_inputs <-
    iris_rec |>
    step_arrange() |>
    prep(training = iris) |>
    bake(new_data = NULL, composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)

  rec <- recipe(~., mtcars) |>
    step_arrange(vs) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$vs))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$am))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_arrange() is one of the thin wrappers around dplyr functions and
  # is thus hard to check against
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_arrange(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_arrange(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_arrange(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- iris_rec |>
    step_arrange(Sepal.Length)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data = data) |>
    step_arrange(Sepal.Length) |>
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
