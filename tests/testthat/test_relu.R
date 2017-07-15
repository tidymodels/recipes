library(testthat)
library(recipes)

df <- tibble(val1 = -10:10, val2 = factor(LETTERS[1:21]))

test_that('default relu settings', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1) %>%
    prepare(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(right_relu_val1 = pmax(rep(0, length(val1)), val1))

  expect_equal(expected_baked, baked)
})


test_that('shifted and reversed relu', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1, shift = 5, reverse = TRUE) %>%
    prepare(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(left_relu_val1 = pmax(rep(0, length(val1)), -(val1 - 5)))

  expect_equal(expected_baked, baked)
})


test_that('reversed softplus', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1, smooth = TRUE, reverse = TRUE) %>%
    prepare(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(left_relu_val1 = log1p(exp(-val1)))

  expect_equal(expected_baked, baked)
})


test_that('shifted and prefixed softplus', {
  baked <- recipe(~ ., data = df) %>%
    step_relu(val1, shift = 5,  smooth = TRUE, prefix = "sp_") %>%
    prepare(df, verbose = FALSE) %>%
    bake(df)

  expected_baked <- df %>%
    dplyr::mutate(sp_val1 = log1p(exp(val1 - 5)))

  expect_equal(expected_baked, baked)
})


test_that('input checking', {
   expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val1, shift = TRUE) %>%  # wrong argument type to shift
      prepare(df, verbose = FALSE),
    "numeric"
  )

  expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val1, reverse = 3) %>%  # wrong argument type to reverse
      prepare(df, verbose = FALSE),
    "logical"
  )

  expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val1, smooth = "cat") %>%  # wrong argument type to smooth
      prepare(df, verbose = FALSE),
    "logical"
  )

  expect_error(
    recipe(~ ., data = df) %>%
      step_relu(val2) %>%  # apply to non-numeric column
      prepare(df, verbose = FALSE),
    "numeric"
  )
})


test_that('printing', {
  rec <- recipe(~ ., data = df) %>%
    step_relu(val1)
  expect_output(print(rec))
  expect_output(prepare(rec, training = df))
})

