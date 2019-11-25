library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

context("dplyr sampling steps")

# ------------------------------------------------------------------------------

iris2 <- iris %>% mutate(row = 1:150)
iris_rec <- recipe( ~ ., data = iris2)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  single_sample <-
    iris_rec %>%
    step_sample(size = 1) %>%
    prep(training = iris2) %>%
    juice() %>%
    nrow()
  expect_equal(single_sample, 1)

  full_sample <-
    iris_rec %>%
    step_sample(size = 0.99999) %>%
    prep(training = iris2) %>%
    juice() %>%
    nrow()
  expect_equal(full_sample, 150)

  half_sample <-
    iris_rec %>%
    step_sample(size = 0.5) %>%
    prep(training = iris2) %>%
    juice() %>%
    nrow()
  expect_equal(half_sample, 75)

  third_sample <-
    iris_rec %>%
    step_sample(size = 50) %>%
    prep(training = iris2) %>%
    juice() %>%
    nrow()
  expect_equal(third_sample, 50)

  whole_sample <-
    iris_rec %>%
    step_sample() %>%
    prep(training = iris2) %>%
    juice() %>%
    nrow()
  expect_equal(whole_sample, 150)

  smaller_iris <-
    iris_rec %>%
    step_sample() %>%
    prep(training = iris2 %>% slice(1:120))

  expect_equal(juice(smaller_iris) %>% nrow(), 120)
  expect_equal(bake(smaller_iris, iris2 %>% slice(121:150)) %>% nrow(), 30)

  boot_sample <-
    iris_rec %>%
    step_sample(replace = TRUE) %>%
    prep(training = iris2) %>%
    juice() %>%
    pull(row) %>%
    table()
  expect_true(max(boot_sample) > 1)
  expect_equal(sum(boot_sample), 150)
})

test_that('bad input', {
  expect_error(iris_rec %>% step_sample(size = -1))
  expect_error(iris_rec %>% step_sample(size = "a"))
  expect_error(iris_rec %>% step_sample(replace = "a"))
})



test_that('printing', {
  rec <- iris_rec %>% step_sample()
  expect_output(print(rec))
  expect_output(prep(rec, training = iris2, verbose = TRUE))
})

