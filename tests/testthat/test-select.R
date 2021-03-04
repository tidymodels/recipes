
test_that("basic usage", {
  rec <-
    recipe(~., data = iris) %>%
    step_select(Species, starts_with("Sepal"))

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select(Species, starts_with("Sepal"))

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris %>%
    as_tibble() %>%
    # change the position of the variables to check that this is not a problem
    select(Species, starts_with("Sepal"), starts_with("Petal")) %>%
    slice(76:150)

  dplyr_test <-
    iris_test %>%
    select(Species, starts_with("Sepal"))

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("basic rename", {
  rec <-
    recipe(~., data = iris) %>%
    step_select(Species, sepal_length = Sepal.Length)

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select(Species, sepal_length = Sepal.Length)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris %>%
    as_tibble() %>%
    # change the position of the variables to check that this is not a problem
    select(Species, starts_with("Sepal"), starts_with("Petal")) %>%
    slice(76:150)

  dplyr_test <-
    iris_test %>%
    select(Species, sepal_length = Sepal.Length)

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("select via type", {
  rec <-
    recipe(~., data = iris) %>%
    step_select(all_numeric())

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select_if(is.numeric)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris %>%
    as_tibble() %>%
    # change the position of the variables to check that this is not a problem
    select(Species, starts_with("Sepal"), starts_with("Petal")) %>%
    slice(76:150)

  dplyr_test <-
    iris_test %>%
    select_if(is.numeric)

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("select via role", {
  rec <-
    recipe(Species ~ ., data = iris) %>%
    step_select(all_predictors())

  prepped <- prep(rec, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select(-Species)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  iris_test <- iris %>%
    as_tibble() %>%
    # change the position of the variables to check that this is not a problem
    select(Species, starts_with("Sepal"), starts_with("Petal")) %>%
    slice(76:150)

  dplyr_test <-
    iris_test %>%
    select(-Species)

  rec_test <- bake(prepped, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  sepal_vars <- c("Sepal.Width", "Sepal.Length")

  rec_1 <-
    recipe(~., data = iris) %>%
    step_select(Species, sepal_vars)

  prepped_1 <- prep(rec_1, training = iris %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    select(Species, sepal_vars)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    recipe(~., data = iris) %>%
    step_select(Species, !!sepal_vars)

  prepped_2 <- prep(rec_2, training = iris %>% slice(1:75))

  rm(sepal_vars)
  expect_error(prep(rec_1, training = iris %>% slice(1:75)))
  expect_error(
    prepped_2 <- prep(rec_2, training = iris %>% slice(1:75)),
    regexp = NA
  )
  rec_2_train <- juice(prepped_2)
  expect_equal(dplyr_train, rec_2_train)
})

test_that("no input", {
  expect_error(
    recipe(~., data = iris) %>%
      step_select() %>%
      prep(training = iris),
    "Please supply at least one variable specification.See [?]selections."
  )
})

test_that("printing", {
  rec <- recipe(~., data = iris) %>%
    step_select(Species, starts_with("Sepal"), petal_width = Petal.Width)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris, verbose = TRUE))
})

test_that("tidying", {
  set.seed(403)
  petal <- c("Petal.Width", "Petal.Length")
  rec <- recipe(~., data = iris) %>%
    step_select(species = Species, starts_with("Sepal"), petal) %>%
    step_select(!!petal)
  prepped <- prep(rec, training = iris %>% slice(1:75))

  verify_output(test_path("print_test_output", "tidy-select-untrained"), {
    tidy(rec, number = 1)
    tidy(rec, number = 2)
  })
  verify_output(test_path("print_test_output", "tidy-select-trained"), {
    tidy(prepped, number = 1)
    tidy(prepped, number = 2)
  })
})
