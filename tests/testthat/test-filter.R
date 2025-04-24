library(testthat)
library(recipes)

# ------------------------------------------------------------------------------

iris_rec <- recipe(~., data = iris)

# ------------------------------------------------------------------------------

test_that("basic usage - skip = FALSE", {
  rec <-
    iris_rec |>
    step_filter(Sepal.Length > 4.5, Species == "setosa", skip = FALSE)

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150) |>
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")
  dplyr_test <- dplyr_test[, names(rec_train)]

  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("skip = FALSE", {
  rec <-
    iris_rec |>
    step_filter(Sepal.Length > 4.5, Species == "setosa", skip = FALSE)

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150) |>
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")
  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  values <- c("versicolor", "virginica")
  rec_1 <-
    iris_rec |>
    step_filter(Sepal.Length > 4.5, Species %in% values)

  prepped_1 <- prep(rec_1, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    filter(Sepal.Length > 4.5, Species %in% values)

  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    iris_rec |>
    step_filter(Sepal.Length > 4.5, Species %in% !!values)

  prepped_2 <- prep(rec_2, training = iris |> slice(1:75))

  expect_no_error(
    prepped_2 <- prep(rec_2, training = iris |> slice(1:75))
  )
  rec_2_train <- bake(prepped_2, new_data = NULL)
  expect_equal(dplyr_train, rec_2_train)
})

test_that("no input", {
  no_inputs <-
    iris_rec |>
    step_filter() |>
    prep(training = iris) |>
    bake(new_data = NULL, composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)

  rec <- recipe(~., mtcars) |>
    step_filter(vs == 0) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$vs))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$am))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_filter() is one of the thin wrappers around dplyr functions and
  # is thus hard to check against
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_filter(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_filter(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_filter(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- iris_rec |>
    step_filter(Sepal.Length > 4.5)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_filter(disp > 100) |>
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
