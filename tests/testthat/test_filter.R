library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

context("dplyr filter steps")

# ------------------------------------------------------------------------------

iris_rec <- recipe( ~ ., data = iris)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    iris_rec %>%
    step_filter(Sepal.Length > 4.5, Species == "setosa")

  prepped <- prep(rec, training = iris %>% slice(1:75), retain = TRUE)

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
  values <- c("versicolor", "virginica")
  rec_1 <-
    iris_rec %>%
    step_filter(Sepal.Length > 4.5, Species  %in% values)

  prepped_1 <- prep(rec_1, training = iris %>% slice(1:75), retain = TRUE)

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    filter(Sepal.Length > 4.5, Species  %in% values)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    iris_rec %>%
    step_filter(Sepal.Length > 4.5, Species  %in% !!values)

  prepped_2 <- prep(rec_2, training = iris %>% slice(1:75), retain = TRUE)

  rm(values)
  expect_error(prep(rec_1, training = iris %>% slice(1:75), retain = TRUE))
  expect_error(
    prepped_2 <- prep(rec_2, training = iris %>% slice(1:75), retain = TRUE),
    regexp = NA
  )
  rec_2_train <- juice(prepped_2)
  expect_equal(dplyr_train, rec_2_train)
})

test_that('no input', {
  no_inputs <-
    iris_rec %>%
    step_filter() %>%
    prep(training = iris, retain = TRUE) %>%
    juice(composition = "data.frame")
  expect_equal(no_inputs, iris)
})


test_that('printing', {
  rec <- iris_rec %>% step_filter(Sepal.Length > 4.5)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris, verbose = TRUE))
})

