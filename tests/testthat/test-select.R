test_that("basic usage", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150) |>
    # change the position of the variables to check that this is not a problem
    select(Species, starts_with("Sepal"), starts_with("Petal"))

  dplyr_train <- select(iris_train, Species, starts_with("Sepal"))
  dplyr_test <- select(iris_test, Species, starts_with("Sepal"))

  rec <- recipe(~., data = iris_train) |>
    step_select(Species, starts_with("Sepal")) |>
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("basic rename", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150)

  dplyr_train <- select(iris_train, Species, sepal_length = Sepal.Length)
  dplyr_test <- select(iris_test, Species, sepal_length = Sepal.Length)

  rec <- recipe(~., data = iris_train) |>
    step_select(Species, sepal_length = Sepal.Length) |>
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("select via type", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150)

  dplyr_train <- select_if(iris_train, is.numeric)
  dplyr_test <- select_if(iris_test, is.numeric)

  rec <- recipe(~., data = iris_train) |>
    step_select(all_numeric()) |>
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("select via role", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)
  iris_test <- slice(iris_tbl, 76:150)

  dplyr_train <- select(iris_train, -Species)
  dplyr_test <- select(iris_test, -Species)

  rec <- recipe(Species ~ ., data = iris_train) |>
    step_select(all_predictors()) |>
    prep(training = iris_train)

  rec_train <- bake(rec, new_data = NULL)
  expect_equal(dplyr_train, rec_train)

  rec_test <- bake(rec, iris_test)
  expect_equal(dplyr_test, rec_test)
})

test_that("quasiquotation", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  # Local variables
  sepal_vars <- c("Sepal.Width", "Sepal.Length")

  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)

  dplyr_train <- select(iris_train, all_of(sepal_vars))

  rec_1 <-
    recipe(~., data = iris_train) |>
    step_select(all_of(sepal_vars))
  rec_2 <-
    recipe(~., data = iris_train) |>
    step_select(all_of(!!sepal_vars))

  # both work when local variable is available
  prepped_1 <- prep(rec_1, training = iris_train)
  rec_1_train <- bake(prepped_1, new_data = NULL)
  expect_equal(dplyr_train, rec_1_train)

  prepped_2 <- prep(rec_2, training = iris_train)
  rec_2_train <- bake(prepped_2, new_data = NULL)
  expect_equal(dplyr_train, rec_2_train)

  prepped_2 <- prep(rec_2, training = iris_train)
  rec_2_train <- bake(prepped_2, new_data = NULL)
  expect_equal(dplyr_train, rec_2_train)
})

test_that("tidying", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  iris_tbl <- as_tibble(iris)
  iris_train <- slice(iris_tbl, 1:75)

  petal <- c("Petal.Width", "Petal.Length")

  set.seed(403)
  rec <- recipe(~., data = iris) |>
    step_select(
      species = Species,
      starts_with("Sepal"),
      all_of(petal),
      id = "select_no_qq"
    ) |>
    step_select(all_of(!!petal), id = "select_qq")
  prepped <- prep(rec, training = iris_train)

  verify_output(test_path("print_test_output", "tidy-select-untrained"), {
    tidy(rec, number = 1)
    tidy(rec, number = 2)
  })
  verify_output(test_path("print_test_output", "tidy-select-trained"), {
    tidy(prepped, number = 1)
    tidy(prepped, number = 2)
  })
})

test_that("doesn't destroy sparsity", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)

  rec <- recipe(~., mtcars) |>
    step_select(vs, mpg, disp) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$vs))
})

test_that("step_select() throws deprecating warning", {
  expect_snapshot(
    tmp <- recipe(~., mtcars) |>
      step_select(vs, mpg, disp)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  rec <- recipe(~., data = mtcars) |>
    step_select(cyl) |>
    update_role(cyl, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep(training = mtcars)

  expect_snapshot(error = TRUE, bake(rec, new_data = mtcars[, c(-2)]))
})

test_that("empty printing", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_select(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  # Here for completeness
  # step_select() will mimick dplyr::select() by not selecting anything
  expect_true(TRUE)
})

test_that("empty selection tidy method works", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_select(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  rec <- recipe(~., data = iris) |>
    step_select(Species)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  data <- mtcars
  rec <- recipe(~., data) |>
    step_select(all_predictors()) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
