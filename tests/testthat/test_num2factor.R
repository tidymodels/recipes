library(recipes)
library(testthat)

# ------------------------------------------------------------------------------

n <- 200

set.seed(8575)
ex_dat <- data.frame(
  w = sample(letters[1:3], size = n, replace = TRUE),
  x = rnorm(n),
  z = rep(1:10, each = 20)
)

rec <- recipe(~ ., data = ex_dat)

test_that('basic functionality', {
  ex_1 <- rec %>%
    step_num2factor(z, levels = rev(LETTERS[1:10])) %>%
    prep(ex_dat) %>%
    juice()
  expect_true(inherits(ex_1$w, "factor"))
  expect_true(inherits(ex_1$x, "numeric"))
  expect_true(inherits(ex_1$z, "factor"))
  expect_equal(levels(ex_1$z), rev(LETTERS[1:10]))

  ex_2 <- rec %>%
    step_num2factor(z, ordered = TRUE, levels = rev(LETTERS[1:10])) %>%
    prep(ex_dat) %>%
    juice
  expect_true(inherits(ex_2$w, "factor"))
  expect_true(inherits(ex_2$x, "numeric"))
  expect_true(inherits(ex_2$z, "ordered"))
  expect_equal(levels(ex_1$z), rev(LETTERS[1:10]))
})

test_that('bad args', {
  expect_error(
    rec %>%
      step_num2factor(w, x, levels = c("one", "two")) %>%
      prep(ex_dat),
    "All columns selected for the step should be numeric"
  )
  expect_error(
    rec %>%
      step_num2factor(w, x) %>%
      prep(ex_dat),
    "Please provide a character vector of"
  )
})


test_that('printing', {
  ex_3 <- rec %>%
    step_num2factor(z, levels = letters) %>%
    prep(ex_dat, strings_as_factors = FALSE)
  expect_output(print(ex_3))
  expect_output(prep(ex_3, training = ex_dat, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_num2factor(rec1, levels = "x")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_num2factor(rec, levels = "x")

  expect <- tibble(terms = character(), ordered = logical(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_num2factor(rec, levels = "x")

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
