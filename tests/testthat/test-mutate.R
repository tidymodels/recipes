library(testthat)
library(recipes)

iris_rec <- recipe(~., data = iris)

test_that("basic usage", {
  rec <-
    iris_rec |>
    step_mutate(
      dbl_width = Sepal.Width * 2,
      half_length = Sepal.Length / 2
    )

  prepped <- prep(rec, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    mutate(
      dbl_width = Sepal.Width * 2,
      half_length = Sepal.Length / 2
    )

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris |>
    as_tibble() |>
    slice(76:150) |>
    mutate(
      dbl_width = Sepal.Width * 2,
      half_length = Sepal.Length / 2
    )
  rec_test <- bake(prepped, iris |> slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  const <- 9.077
  rec_1 <-
    iris_rec |>
    step_mutate(new_var = Sepal.Width * const)

  prepped_1 <- prep(rec_1, training = iris |> slice(1:75))

  dplyr_train <-
    iris |>
    as_tibble() |>
    slice(1:75) |>
    mutate(new_var = Sepal.Width * const)

  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    iris_rec |>
    step_mutate(new_var = Sepal.Width * !!const)

  prepped_2 <- prep(rec_2, training = iris |> slice(1:75))

  rm(const)
  expect_snapshot(error = TRUE, prep(rec_1, training = iris |> slice(1:75)))
  expect_no_error(
    prepped_2 <- prep(rec_2, training = iris |> slice(1:75))
  )
  rec_2_train <- bake(prepped_2, new_data = NULL)
  expect_equal(dplyr_train, rec_2_train)
})

test_that("can use unnamed expressions like `across()` (#759)", {
  skip_if_not_installed("dplyr", "1.0.0")

  df <- tibble(
    x = c(TRUE, FALSE),
    y = c(1, 2),
    z = c(TRUE, FALSE)
  )

  rec <- recipe(~., df) |>
    step_mutate(across(where(is.logical), as.integer))

  rec <- prep(rec, df)

  expect_identical(
    bake(rec, new_data = NULL),
    mutate(df, across(where(is.logical), as.integer))
  )
})

test_that("tidying allows for named and unnamed expressions", {
  rec <- step_mutate(iris_rec, x = mean(y), id = "named")
  tidied <- tidy(rec, id = "named")

  # Named expressions use the name
  expect_identical(tidied$terms, "x")
  expect_identical(tidied$value, "mean(y)")

  rec <- step_mutate(iris_rec, across(c(x, y), mean), id = "unnamed")
  tidied <- tidy(rec, id = "unnamed")

  # Unnamed expressions use the expression
  expect_identical(tidied$terms, "across(c(x, y), mean)")
  expect_identical(tidied$value, "across(c(x, y), mean)")
})

test_that("required_pkgs.step_mutate() works", {
  rec <- recipe(~., data = mtcars) |>
    step_mutate(new = 2)

  rec_pred <- prep(rec)

  expect_equal(required_pkgs(rec), "recipes")
  expect_equal(required_pkgs(rec_pred), "recipes")

  rec <- recipe(~., data = mtcars) |>
    step_mutate(new = 2, .pkgs = "stats")

  rec_pred <- prep(rec)

  expect_equal(required_pkgs(rec), c("recipes", "stats"))
  expect_equal(required_pkgs(rec_pred), c("recipes", "stats"))

  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_mutate(new = 2, .pkgs = "not-a-package")
  )
})

test_that("step_mutate() .pkgs argument is backwards compatible", {
  rec <- recipe(~., data = mtcars) |>
    step_mutate(new = 2)

  rec$steps[[1]]$.pkgs <- NULL

  rec_pred <- prep(rec)

  expect_equal(required_pkgs(rec), "recipes")
  expect_equal(required_pkgs(rec_pred), "recipes")

  rec <- recipe(~., data = mtcars) |>
    step_mutate(new = 2)

  rec_pred <- prep(rec)

  rec_pred$steps[[1]]$.pkgs <- NULL

  expect_equal(required_pkgs(rec), "recipes")
  expect_equal(required_pkgs(rec_pred), "recipes")
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_rename() is one of the thin wrappers around dplyr functions and
  # is thus hard to check against
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_mutate(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_mutate(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("printing", {
  rec <- recipe(~., data = iris) |>
    step_mutate(x = 5)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_mutate(disp = disp / 4) |>
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
