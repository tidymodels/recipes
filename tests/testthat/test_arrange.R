library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

iris_rec <- recipe( ~ ., data = iris)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    iris_rec %>%
    step_arrange(desc(Sepal.Length), 1/Petal.Length)

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::arrange(desc(Sepal.Length), 1/Petal.Length)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::arrange(desc(Sepal.Length), 1/Petal.Length)
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
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

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

})

test_that('no input', {
  no_inputs <-
    iris_rec %>%
    step_arrange() %>%
    prep(training = iris) %>%
    juice(composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that('printing', {
  rec <- iris_rec %>% step_arrange(Sepal.Length)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris, verbose = TRUE))
})

