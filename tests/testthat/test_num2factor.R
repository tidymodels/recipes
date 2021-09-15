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


