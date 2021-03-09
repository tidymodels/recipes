
test_that("basic usage", {
  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150) %>%
    # change the position of the variables to check that this is not a problem
    select(Species, starts_with("Sepal"), starts_with("Petal"))

  dplyr_train <- select(iris_train, Species, starts_with("Sepal"))
  dplyr_test <- select(iris_test, Species, starts_with("Sepal"))

  rec <- recipe(~., data = iris_train) %>%
    step_select(Species, starts_with("Sepal")) %>%
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("basic rename", {
  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150)

  dplyr_train <- select(iris_train, Species, sepal_length = Sepal.Length)
  dplyr_test <- select(iris_test, Species, sepal_length = Sepal.Length)

  rec <- recipe(~., data = iris_train) %>%
    step_select(Species, sepal_length = Sepal.Length) %>%
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("select via type", {
  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150)

  dplyr_train <- select_if(iris_train, is.numeric)
  dplyr_test <- select_if(iris_test, is.numeric)

  rec <- recipe(~., data = iris_train) %>%
    step_select(all_numeric()) %>%
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("select via role", {
  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150)

  dplyr_train <- select(iris_train, -Species)
  dplyr_test <- select(iris_test, -Species)

  rec <- recipe(Species ~ ., data = iris_train) %>%
    step_select(all_predictors()) %>%
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  # Local variables
  sepal_vars <- c("Sepal.Width", "Sepal.Length")

  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)

  dplyr_train <- select(iris_train, all_of(sepal_vars))

  rec_1 <-
    recipe(~., data = iris_train) %>%
    step_select(all_of(sepal_vars))
  rec_2 <-
    recipe(~., data = iris_train) %>%
    step_select(all_of(!!sepal_vars))

  # both work when local variable is available
  prepped_1 <- prep(rec_1, training = iris_train)
  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)

  prepped_2 <- prep(rec_2, training = iris_train)
  rec_2_train <- bake(prepped_2, new_data = NULL)
  expect_equal(dplyr_train, rec_2_train)

  # only rec_2 works when local variable is removed
  rm(sepal_vars)

  expect_error(prep(rec_1, training = iris_train))

  prepped_2 <- prep(rec_2, training = iris_train)
  rec_2_train <- bake(prepped_2, new_data = NULL)
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
  # the RNGkind() changed in R version 3.6 so we set it to v3.5 so that the id
  # in the tidy tibble is consistent across older and newer versions of R
  r_version <- getRversion() %>%
    as.character() %>%
    strsplit(".", fixed = TRUE)
  r_version <- r_version[[1]] %>% as.numeric()
  r_version_pre_36 <- r_version[1] > 3 | (r_version[1] >= 3 & r_version[2] > 5)

  if (r_version_pre_36) {
    suppressWarnings(RNGversion("3.5.0"))
  }

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
  # set RNG version back to default for R versions > 3.5

  if (r_version_pre_36){
    RNGversion(getRversion())
  }
})
