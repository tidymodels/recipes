library(testthat)
library(recipes)
library(dplyr)

# ------------------------------------------------------------------------------

iris_rec <- recipe(~., data = iris)

# ------------------------------------------------------------------------------

test_that("basic usage", {
  rec <-
    iris_rec %>%
    step_mutate(
      dbl_width = Sepal.Width * 2,
      half_length = Sepal.Length / 2
    )

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(
      dbl_width = Sepal.Width * 2,
      half_length = Sepal.Length / 2
    )

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    mutate(
      dbl_width = Sepal.Width * 2,
      half_length = Sepal.Length / 2
    )
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  const <- 9.077
  rec_1 <-
    iris_rec %>%
    step_mutate(new_var = Sepal.Width * const)

  prepped_1 <- prep(rec_1, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(new_var = Sepal.Width * const)

  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    iris_rec %>%
    step_mutate(new_var = Sepal.Width * !!const)

  prepped_2 <- prep(rec_2, training = iris %>% slice(1:75))

  rm(const)
  expect_snapshot(error = TRUE,
    prep(rec_1, training = iris %>% slice(1:75))
  )
  expect_error(
    prepped_2 <- prep(rec_2, training = iris %>% slice(1:75)),
    regexp = NA
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

  rec <- recipe(~., df) %>%
    step_mutate(across(where(is.logical), as.integer))

  rec <- prep(rec, df)

  expect_identical(
    bake(rec, new_data = NULL),
    mutate(df, across(where(is.logical), as.integer))
  )
})

test_that("no input", {
  no_inputs <-
    iris_rec %>%
    step_mutate() %>%
    prep(training = iris) %>%
    bake(new_data = NULL, composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that("printing", {
  rec <- iris_rec %>% step_mutate(x = 5)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
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

# ------------------------------------------------------------------------------

test_that("basic usage", {
  rec <-
    iris_rec %>%
    step_mutate_at(contains("Length"), fn = log)

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(
      Sepal.Length = log(Sepal.Length),
      Petal.Length = log(Petal.Length)
    )

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    mutate(
      Sepal.Length = log(Sepal.Length),
      Petal.Length = log(Petal.Length)
    )
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that("mulitple functions", {
  rec <-
    iris_rec %>%
    step_mutate_at(contains("Length"), fn = list(a = log, b = sqrt))

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(
      Sepal.Length_a = log(Sepal.Length),
      Petal.Length_a = log(Petal.Length),
      Sepal.Length_b = sqrt(Sepal.Length),
      Petal.Length_b = sqrt(Petal.Length)
    )

  rec_train <- bake(prepped, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    mutate(
      Sepal.Length_a = log(Sepal.Length),
      Petal.Length_a = log(Petal.Length),
      Sepal.Length_b = sqrt(Sepal.Length),
      Petal.Length_b = sqrt(Petal.Length)
    )
  rec_test <- bake(prepped, iris %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})


test_that("no input", {
  # Wait for call pass through
  expect_error(
    iris_rec %>%
      step_mutate_at() %>%
      prep(training = iris) %>%
      bake(new_data = NULL, composition = "data.frame")
  )
})

test_that("printing", {
  rec <- iris_rec %>% step_mutate_at(contains("Sepal"), fn = log)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("mutate_at - empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_mutate_at(rec1, fn = mean)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("mutate_at - empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_mutate_at(rec, fn = mean)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("mutate_at - empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_mutate_at(rec, fn = mean)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
