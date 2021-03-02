library(testthat)
library(recipes)
library(tibble)

context("Removing variables")


test_that("basics", {

  n <- 20
  set.seed(12)
  ex_dat <- data.frame(
    x1 = rnorm(n),
    x2 = runif(n)
  )

  rec <- recipe(~., data = ex_dat) %>%
    step_rm(x1)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_rm <- bake(rec_trained, new_data = ex_dat)

  expect_equal(colnames(rec_rm), "x2")
})

# test_that('skipping', {
#
#   n <- 20
#   set.seed(12)
#   ex_dat <- data.frame(
#     x1 = rnorm(n),
#     x2 = runif(n)
#   )
#
#   rec <- recipe(~., data = ex_dat) %>%
#     step_rm(x1, skip = TRUE)
#
#   rec_trained <- prep(rec, training = ex_dat)
#   tr_res <- juice(rec_trained)
#   te_res <- bake(rec_trained, new_data = ex_dat)
#
#   expect_equal(colnames(tr_res), "x2")
#   expect_equal(colnames(tr_res), c("x1", "x2"))
# })


test_that("basic usage", {
  rec <-
    recipe(~., data = iris) %>%
    step_rm(Species, starts_with("Sepal"))

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select(-Species, -starts_with("Sepal"))

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris %>%
    as_tibble() %>%
    # change the position of the variables
    select(Species, starts_with("Sepal"), starts_with("Petal")) %>%
    slice(76:150)
  dplyr_test <-
    iris_test %>%
    select(-Species, -starts_with("Sepal"))

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("basic rename", {
  rec <-
    recipe(~., data = iris) %>%
    step_rm(sepal_length = Sepal.Length)

  expect_error(
    prep(rec, training = iris %>% slice(1:75)),
    "Can't rename variables in this context."
  )
})

test_that("remove via type", {
  rec <-
    recipe(~., data = iris) %>%
    step_rm(all_numeric())

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select_if(~ !is.numeric(.))

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris %>%
    as_tibble() %>%
    # change the position of the variables
    select(Species, starts_with("Sepal"), starts_with("Petal")) %>%
    slice(76:150)
  dplyr_test <-
    iris_test %>%
    select_if(~ !is.numeric(.))

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("remove via role", {
  rec <-
    recipe(Species ~ ., data = iris) %>%
    step_rm(all_predictors())

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select(Species)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris %>%
    as_tibble() %>%
    # change the position of the variables
    select(Species, starts_with("Sepal"), starts_with("Petal")) %>%
    slice(76:150)
  dplyr_test <-
    iris_test %>%
    select(Species)

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("remove with quasi-quotation", {
  sepal_vars <- c("Sepal.Width", "Sepal.Length")

  rec_1 <-
    recipe(~., data = iris) %>%
    step_rm(all_of(sepal_vars))

  prepped_1 <- prep(rec_1, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select(-sepal_vars)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    recipe(~., data = iris) %>%
    step_rm(!!sepal_vars)

  prepped_2 <- prep(rec_2, training = iris %>% slice(1:75))

  rm(sepal_vars)
  expect_error(prep(rec_1, training = iris %>% slice(1:75)))
  # expect_error(
  #   prepped_2 <- prep(rec_2, training = iris %>% slice(1:75)),
  #   regexp = NA
  # )
  rec_2_train <- juice(prepped_2)
  expect_equal(dplyr_train, rec_2_train)
})

test_that("no input", {
  expect_error(
    recipe(~., data = iris) %>%
      step_rm() %>%
      prep(training = iris),
    "Please supply at least one variable specification.See [?]selections."
  )
})

test_that("printing", {

  n <- 20
  set.seed(12)
  ex_dat <- data.frame(
    x1 = rnorm(n),
    x2 = runif(n)
  )

  rec <- recipe(~., data = ex_dat) %>%
    step_rm(x1)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})

