library(testthat)
library(recipes)

# ------------------------------------------------------------------------------

iris2 <- iris |> mutate(row = 1:150)
iris_rec <- recipe(~., data = iris2)

# ------------------------------------------------------------------------------

test_that("basic usage", {
  single_sample <-
    iris_rec |>
    step_sample(size = 1) |>
    prep(training = iris2) |>
    bake(new_data = NULL) |>
    nrow()
  expect_equal(single_sample, 1)

  full_sample <-
    iris_rec |>
    step_sample(size = 0.99999) |>
    prep(training = iris2) |>
    bake(new_data = NULL) |>
    nrow()
  expect_equal(full_sample, 150)

  half_sample <-
    iris_rec |>
    step_sample(size = 0.5) |>
    prep(training = iris2) |>
    bake(new_data = NULL) |>
    nrow()
  expect_equal(half_sample, 75)

  third_sample <-
    iris_rec |>
    step_sample(size = 50) |>
    prep(training = iris2) |>
    bake(new_data = NULL) |>
    nrow()
  expect_equal(third_sample, 50)

  whole_sample <-
    iris_rec |>
    step_sample() |>
    prep(training = iris2) |>
    bake(new_data = NULL) |>
    nrow()
  expect_equal(whole_sample, 150)

  smaller_iris <-
    iris_rec |>
    step_sample() |>
    prep(training = iris2 |> slice(1:120))

  expect_equal(bake(smaller_iris, new_data = NULL) |> nrow(), 120)
  expect_equal(bake(smaller_iris, iris2 |> slice(121:150)) |> nrow(), 30)

  boot_sample <-
    iris_rec |>
    step_sample(replace = TRUE) |>
    prep(training = iris2) |>
    bake(new_data = NULL) |>
    pull(row) |>
    table()
  expect_true(max(boot_sample) > 1)
  expect_equal(sum(boot_sample), 150)
})

test_that("bad input", {
  expect_snapshot(error = TRUE, iris_rec |> step_sample(size = -1) |> prep())
  expect_snapshot(error = TRUE, iris_rec |> step_sample(size = "a") |> prep())
  expect_snapshot(
    error = TRUE,
    iris_rec |> step_sample(replace = "a") |> prep()
  )
})

test_that("sample with case weights", {
  mtcars1 <- mtcars
  mtcars1$carb <- frequency_weights(mtcars1$carb)

  # sample_n
  set.seed(1234)
  rec <-
    recipe(~., mtcars1) |>
    step_sample(size = 10, id = "") |>
    prep()

  set.seed(1234)
  exp_res <- sample_n(
    as_tibble(mtcars1),
    size = 10,
    weight = mtcars1$carb
  )

  expect_equal(
    bake(rec, new_data = NULL),
    exp_res
  )

  # sample_frac
  set.seed(1234)
  rec <-
    recipe(~., mtcars1) |>
    step_sample(size = 0.5, id = "") |>
    prep()

  set.seed(1234)
  exp_res <- sample_frac(
    as_tibble(mtcars1),
    size = 0.5,
    weight = mtcars1$carb
  )

  expect_equal(
    bake(rec, new_data = NULL),
    exp_res
  )

  expect_snapshot(rec)

  # Wrong weights
  mtcars2 <- mtcars
  mtcars2$carb <- importance_weights(mtcars2$carb)

  rec <-
    recipe(~., mtcars1) |>
    step_sample(size = 10, id = "") |>
    prep()

  expect_snapshot(rec)
})

test_that("warn when selectors are provided", {
  expect_snapshot(
    tmp <- recipe(~., data = mtcars) |>
      step_sample(all_predictors())
  )
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)

  rec <- recipe(~., mtcars) |>
    step_sample(size = 10) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$vs))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$am))

  rec <- recipe(~., mtcars) |>
    step_sample(size = 0.5) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$vs))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$am))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_sample() is special as it doesn't care about the incoming data
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_sample(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_sample(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  # Here for completeness
  # step_sample() is special as it can't be used without selection
  expect_true(TRUE)
})

test_that("printing", {
  rec <- recipe(~., data = iris) |>
    step_sample()

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_sample() |>
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
