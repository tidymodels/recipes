library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

context("dplyr rename steps")

# ------------------------------------------------------------------------------

iris_rec <- recipe( ~ ., data = iris)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    iris_rec %>%
    step_rename(
      popcorn = Sepal.Width,
      plum = Sepal.Length
    )

  prepped <- prep(rec, training = iris %>% slice(1:75), retain = TRUE)

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    rename(
      popcorn = Sepal.Width,
      plum = Sepal.Length
    )

  rec_train <- juice(prepped)
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

test_that('no input', {
  no_inputs <-
    iris_rec %>%
    step_rename() %>%
    prep(training = iris, retain = TRUE) %>%
    juice(composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that('printing', {
  rec <- iris_rec %>% step_rename(wat = Species)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris, verbose = TRUE))
})

# ------------------------------------------------------------------------------

context("dplyr rename_at steps")

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    iris_rec %>%
    step_rename_at(contains("Length"), fn = ~ tolower(.))

  prepped <- prep(rec, training = iris %>% slice(1:75), retain = TRUE)

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    rename_at(vars(contains("Length")), ~ tolower(.))

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    rename_at(vars(contains("Length")), ~ tolower(.))
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('mulitple functions', {
  rec <-
    iris_rec %>%
    step_rename_at(contains("Length"), fn = list(a = log, b = sqrt))

  expect_error(prep(rec, training = iris %>% slice(1:75), retain = TRUE))

})


test_that('no input', {
  expect_error(
    iris_rec %>%
      step_rename_at() %>%
      prep(training = iris, retain = TRUE) %>%
      juice(composition = "data.frame")
  )
})

test_that('printing', {
  rec <- iris_rec %>% step_rename_at(contains("Sepal"), fn = tolower)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris, verbose = TRUE))
})

