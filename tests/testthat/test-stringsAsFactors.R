library(testthat)
library(recipes)

n <- 20

set.seed(752)
as_fact <- data.frame(
  numbers = rnorm(n),
  fact = factor(sample(letters[1:3], n, replace = TRUE)),
  ord = factor(sample(LETTERS[22:26], n, replace = TRUE), ordered = TRUE),
  num = seq_len(n)
)
as_str <- as_fact
as_str$fact <- as.character(as_str$fact)
as_str$ord <- as.character(as_str$ord)

test_that("strings_as_factors = FALSE", {
  rec1 <- recipe(~., data = as_fact, strings_as_factors = FALSE) |>
    step_center(numbers)
  rec1 <- prep(
    rec1,
    training = as_fact,
    verbose = FALSE
  )
  rec1_as_fact <- bake(rec1, new_data = as_fact)
  expect_snapshot(rec1_as_str <- bake(rec1, new_data = as_str))
  expect_equal(as_fact$fact, rec1_as_fact$fact)
  expect_equal(as_fact$ord, rec1_as_fact$ord)
  expect_equal(as_fact$num, rec1_as_fact$num)
  expect_equal(as_str$fact, rec1_as_str$fact)
  expect_equal(as_str$ord, rec1_as_str$ord)
  expect_equal(as_str$num, rec1_as_str$num)
})

test_that("strings_as_factors = TRUE", {
  rec2 <- recipe(~., data = as_fact, strings_as_factors = TRUE) |>
    step_center(numbers)
  rec2 <- prep(
    rec2,
    training = as_fact,
    verbose = FALSE
  )
  rec2_as_fact <- bake(rec2, new_data = as_fact)
  expect_snapshot(rec2_as_str <- bake(rec2, new_data = as_str))
  expect_equal(as_fact$fact, rec2_as_fact$fact)
  expect_equal(as_fact$ord, rec2_as_fact$ord)
  expect_equal(as_fact$num, rec2_as_fact$num)
  expect_equal(as_fact$fact, rec2_as_str$fact)
  expect_equal(as_fact$ord, rec2_as_str$ord)
  expect_equal(as_fact$num, rec2_as_str$num)
})

test_that("strings_as_factors = FALSE and zero row input", {
  as_fact <- as_fact[0, ]
  as_str <- as_str[0, ]
  rec1 <- recipe(~., data = as_fact, strings_as_factors = FALSE) |>
    step_center(numbers)
  rec1 <- prep(
    rec1,
    training = as_fact,
    verbose = FALSE
  )
  rec1_as_fact <- bake(rec1, new_data = as_fact)
  expect_snapshot(rec1_as_str <- bake(rec1, new_data = as_str))
  expect_equal(as_fact$fact, rec1_as_fact$fact)
  expect_equal(as_fact$ord, rec1_as_fact$ord)
  expect_equal(as_fact$num, rec1_as_fact$num)
  expect_equal(as_str$fact, rec1_as_str$fact)
  expect_equal(as_str$ord, rec1_as_str$ord)
  expect_equal(as_str$num, rec1_as_str$num)
})

test_that("strings_as_factors = TRUE and zero row input", {
  as_fact <- as_fact[0, ]
  as_str <- as_str[0, ]
  rec2 <- recipe(~., data = as_fact, strings_as_factors = TRUE) |>
    step_center(numbers)
  rec2 <- prep(
    rec2,
    training = as_fact,
    verbose = FALSE
  )
  rec2_as_fact <- bake(rec2, new_data = as_fact)
  expect_snapshot(rec2_as_str <- bake(rec2, new_data = as_str))
  expect_equal(as_fact$fact, rec2_as_fact$fact)
  expect_equal(as_fact$ord, rec2_as_fact$ord)
  expect_equal(as_fact$num, rec2_as_fact$num)
  expect_equal(as_fact$fact, rec2_as_str$fact)
  expect_equal(as_fact$ord, rec2_as_str$ord)
  expect_equal(as_fact$num, rec2_as_str$num)
})
