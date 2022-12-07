library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

iris_rec <- recipe(~., data = iris)

# ------------------------------------------------------------------------------

test_that("basic usage", {
  rec <-
    iris_rec %>%
    step_arrange(desc(Sepal.Length), 1 / Petal.Length)

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::arrange(desc(Sepal.Length), 1 / Petal.Length)
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  sort_vars <- c("Sepal.Length", "Petal.Length")
  sort_vars <- syms(sort_vars)
  rec_1 <-
    iris_rec %>%
    step_arrange(!!!sort_vars)

  prepped_1 <- prep(rec_1, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    arrange(Sepal.Length, Petal.Length)

  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)
})

test_that("no input", {
  no_inputs <-
    iris_rec %>%
    step_arrange() %>%
    prep(training = iris) %>%
    bake(new_data = NULL, composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that("empty tidying", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_arrange(rec)
  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_arrange(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("printing", {
  rec <- iris_rec %>% step_arrange(Sepal.Length)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
