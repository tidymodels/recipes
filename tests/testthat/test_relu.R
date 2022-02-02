library(testthat)
library(recipes)

df <- tibble(val1 = -10:10, val2 = factor(LETTERS[1:21]))

test_that('default relu settings', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1) %>%
    prep(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(right_relu_val1 = pmax(rep(0, length(val1)), val1))

  expect_equal(baked, expected_baked)
})


test_that('shifted and reversed relu', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1, shift = 5, reverse = TRUE) %>%
    prep(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(left_relu_val1 = pmax(rep(0, length(val1)), -(val1 - 5)))

  expect_equal(baked, expected_baked)
})


test_that('reversed softplus', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1, smooth = TRUE, reverse = TRUE) %>%
    prep(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(left_relu_val1 = log1p(exp(-val1)))

  expect_equal(baked, expected_baked)
})


test_that('shifted and prefixed softplus', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1, shift = 5, smooth = TRUE, prefix = "sp_") %>%
    prep(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(sp_val1 = log1p(exp(val1 - 5)))

  expect_equal(baked, expected_baked)
})

test_that('works with all_predictors() selector', {

  expect_silent({
    rec <- recipe(Species ~ ., data = iris) %>%
    step_relu(all_predictors())
  })

  expect_silent(prepped_rec <- prep(rec, iris))
})


test_that('input checking', {
   expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val1, shift = TRUE) %>%  # wrong argument type to shift
      prep(df, verbose = FALSE),
    "numeric"
  )

  expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val1, reverse = 3) %>%  # wrong argument type to reverse
      prep(df, verbose = FALSE),
    "logical"
  )

  expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val1, smooth = "cat") %>%  # wrong argument type to smooth
      prep(df, verbose = FALSE),
    "logical"
  )

  expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val2) %>%  # apply to non-numeric column
      prep(df, verbose = FALSE),
    "numeric"
  )
})

test_that('prints something', {
  rec <- recipe(~ ., data = df) %>%
    step_relu(val1)
  expect_output(print(rec))
  expect_output(prep(rec, training = df, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_relu(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_relu(rec)

  expect <- tibble(terms = character(), shift = double(), reverse = logical(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_relu(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

rm(df)
