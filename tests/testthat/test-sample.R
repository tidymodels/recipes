library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

iris2 <- iris %>% mutate(row = 1:150)
iris_rec <- recipe(~., data = iris2)

# ------------------------------------------------------------------------------

test_that("basic usage", {
  single_sample <-
    iris_rec %>%
    step_sample(size = 1) %>%
    prep(training = iris2) %>%
    bake(new_data = NULL) %>%
    nrow()
  expect_equal(single_sample, 1)

  full_sample <-
    iris_rec %>%
    step_sample(size = 0.99999) %>%
    prep(training = iris2) %>%
    bake(new_data = NULL) %>%
    nrow()
  expect_equal(full_sample, 150)

  half_sample <-
    iris_rec %>%
    step_sample(size = 0.5) %>%
    prep(training = iris2) %>%
    bake(new_data = NULL) %>%
    nrow()
  expect_equal(half_sample, 75)

  third_sample <-
    iris_rec %>%
    step_sample(size = 50) %>%
    prep(training = iris2) %>%
    bake(new_data = NULL) %>%
    nrow()
  expect_equal(third_sample, 50)

  whole_sample <-
    iris_rec %>%
    step_sample() %>%
    prep(training = iris2) %>%
    bake(new_data = NULL) %>%
    nrow()
  expect_equal(whole_sample, 150)

  smaller_iris <-
    iris_rec %>%
    step_sample() %>%
    prep(training = iris2 %>% slice(1:120))

  expect_equal(bake(smaller_iris, new_data = NULL) %>% nrow(), 120)
  expect_equal(bake(smaller_iris, iris2 %>% slice(121:150)) %>% nrow(), 30)

  boot_sample <-
    iris_rec %>%
    step_sample(replace = TRUE) %>%
    prep(training = iris2) %>%
    bake(new_data = NULL) %>%
    pull(row) %>%
    table()
  expect_true(max(boot_sample) > 1)
  expect_equal(sum(boot_sample), 150)
})

test_that("bad input", {
  expect_snapshot(error = TRUE,
    iris_rec %>% step_sample(size = -1)
  )
  expect_snapshot(error = TRUE,
    iris_rec %>% step_sample(size = "a")
  )
  expect_snapshot(error = TRUE,
    iris_rec %>% step_sample(replace = "a")
  )
})

test_that("printing", {
  rec <- iris_rec %>% step_sample()
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("sample with case weights", {
  mtcars1 <- mtcars
  mtcars1$carb <- frequency_weights(mtcars1$carb)

  # sample_n
  set.seed(1234)
  rec <-
    recipe(~ ., mtcars1) %>%
    step_sample(size = 10, id = "") %>%
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
    recipe(~ ., mtcars1) %>%
    step_sample(size = 0.5, id = "") %>%
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
    recipe(~ ., mtcars1) %>%
    step_sample(size = 10, id = "") %>%
    prep()

  expect_snapshot(rec)
})
