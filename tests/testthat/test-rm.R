library(testthat)
library(recipes)

test_that("basics", {
  n <- 20
  set.seed(12)
  ex_dat <- data.frame(
    x1 = rnorm(n),
    x2 = runif(n)
  )

  rec <- recipe(~., data = ex_dat) |>
    step_rm(x1)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_rm <- bake(rec_trained, new_data = ex_dat)

  expect_equal(colnames(rec_rm), "x2")
})

# test_that('skipping', {
#
#   n <- 20
#   set.seed(12)
#   ex_dat <- data.frame(
#     x1 = rnorm(n),
#     x2 = runif(n)
#   )
#
#   rec <- recipe(~., data = ex_dat) |>
#     step_rm(x1, skip = TRUE)
#
#   rec_trained <- prep(rec, training = ex_dat)
#   tr_res <- bake(rec_trained, new_data = NULL)
#   te_res <- bake(rec_trained, new_data = ex_dat)
#
#   expect_equal(colnames(tr_res), "x2")
#   expect_equal(colnames(tr_res), c("x1", "x2"))
# })

test_that("basic usage", {
  rec <-
    recipe(~., data = iris) |>
    step_rm(Species, starts_with("Sepal"))

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    select(-Species, -starts_with("Sepal"))

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris |>
    as_tibble() |>
    # change the position of the variables
    select(Species, starts_with("Sepal"), starts_with("Petal")) |>
    slice(76:150)
  dplyr_test <-
    iris_test |>
    select(-Species, -starts_with("Sepal"))

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("basic rename", {
  rec <-
    recipe(~., data = iris) |>
    step_rm(sepal_length = Sepal.Length)

  expect_snapshot(error = TRUE, prep(rec, training = iris |> slice(1:75)))
})

test_that("remove via type", {
  rec <-
    recipe(~., data = iris) |>
    step_rm(all_numeric())

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    select_if(~ !is.numeric(.))

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris |>
    as_tibble() |>
    # change the position of the variables
    select(Species, starts_with("Sepal"), starts_with("Petal")) |>
    slice(76:150)
  dplyr_test <-
    iris_test |>
    select_if(~ !is.numeric(.))

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("remove via role", {
  rec <-
    recipe(Species ~ ., data = iris) |>
    step_rm(all_predictors())

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    select(Species)

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris |>
    as_tibble() |>
    # change the position of the variables
    select(Species, starts_with("Sepal"), starts_with("Petal")) |>
    slice(76:150)
  dplyr_test <-
    iris_test |>
    select(Species)

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("remove with quasi-quotation", {
  sepal_vars <- c("Sepal.Width", "Sepal.Length")

  rec_1 <-
    recipe(~., data = iris) |>
    step_rm(all_of(sepal_vars))

  prepped_1 <- prep(rec_1, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    select(-all_of(sepal_vars))

  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    recipe(~., data = iris) |>
    step_rm(!!sepal_vars)

  prepped_2 <- prep(rec_2, training = iris |> slice(1:75))

  # expect_no_error(
  #   prepped_2 <- prep(rec_2, training = iris |> slice(1:75))
  # )
  rec_2_train <- bake(prepped_2, new_data = NULL)
  expect_equal(dplyr_train, rec_2_train)
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)

  rec <- recipe(~ vs + am, mtcars) |>
    step_rm(vs) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$am))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_rm() removes variables and thus does not care if they are not there.
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_rm(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_rm(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_rm(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = mtcars) |>
    step_rm(disp)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_rm(mpg, disp) |>
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
