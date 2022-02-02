library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

iris_rec <- recipe( ~ ., data = iris)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    iris_rec %>%
    step_slice(1:5)

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    slice(1:5)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150)
  dplyr_test <- dplyr_test[, names(rec_train)]
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})


test_that('skip = FALSE', {
  rec <-
    iris_rec %>%
    step_slice(1:5, skip = FALSE)

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    slice(1:5)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    slice(1:5)
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
  values <- 1:5
  rec_1 <-
    iris_rec %>%
    step_slice(values)

  prepped_1 <- prep(rec_1, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    slice(values)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  expect_error(
    rec_2 <-
      iris_rec %>%
      step_slice(!!values),
    regexp = NA
  )

  prepped_2 <- prep(rec_2, training = iris %>% slice(1:75))

  rm(values)
  expect_error(prep(rec_1, training = iris %>% slice(1:75)))
  expect_error(
    prepped_2 <- prep(rec_2, training = iris %>% slice(1:75)),
    regexp = NA
  )
  rec_2_train <- juice(prepped_2)
  expect_equal(dplyr_train, rec_2_train)
})


test_that('no input', {
  no_inputs <-
    iris_rec %>%
    step_slice() %>%
    prep(training = iris) %>%
    juice(composition = "data.frame")
  expect_equal(no_inputs, iris)
})


test_that('printing', {
  rec <- iris_rec %>% step_slice(1:2)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_slice(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_slice(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_slice(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
