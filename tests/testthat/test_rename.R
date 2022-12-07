library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

iris_rec <- recipe(~., data = iris)

# ------------------------------------------------------------------------------

test_that("basic usage", {
  rec <-
    iris_rec %>%
    step_rename(
      popcorn = Sepal.Width,
      plum = Sepal.Length
    )

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    rename(
      popcorn = Sepal.Width,
      plum = Sepal.Length
    )

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    rename(
      popcorn = Sepal.Width,
      plum = Sepal.Length
    )
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("no input", {
  no_inputs <-
    iris_rec %>%
    step_rename() %>%
    prep(training = iris) %>%
    bake(new_data = NULL, composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that("printing", {
  rec <- iris_rec %>% step_rename(wat = Species)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("rename - empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_rename(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("rename - empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_rename(rec)

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("rename - empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_rename(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

# ------------------------------------------------------------------------------

test_that("basic usage", {
  rec <-
    iris_rec %>%
    step_rename_at(contains("Length"), fn = ~ tolower(.))

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    rename_at(vars(contains("Length")), ~ tolower(.))

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    rename_at(vars(contains("Length")), ~ tolower(.))
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("mulitple functions", {
  rec <-
    iris_rec %>%
    step_rename_at(contains("Length"), fn = list(a = log, b = sqrt))

  expect_snapshot(error = TRUE,
    prep(rec, training = iris %>% slice(1:75))
  )
})


test_that("no input", {
  # Wait for call pass through
  expect_error(
    iris_rec %>%
      step_rename_at() %>%
      prep(training = iris) %>%
      bake(new_data = NULL, composition = "data.frame")
  )
})

test_that("printing", {
  rec <- iris_rec %>% step_rename_at(contains("Sepal"), fn = tolower)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("rename_at - empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_rename_at(rec1, fn = identity)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("rename_at - empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_rename_at(rec, fn = identity)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("rename_at - empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_rename_at(rec, fn = identity)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
