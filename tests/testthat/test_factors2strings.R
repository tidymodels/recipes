library(recipes)
library(testthat)


n <- 200

set.seed(8575)
ex_dat <- data.frame(
  w = sample(letters[1:3], size = n, replace = TRUE),
  x = sample(LETTERS[1:2], size = n, replace = TRUE),
  y = factor(rep_len(month.abb, n)),
  z = factor(rep_len(month.name, n), ordered = TRUE),
  stringsAsFactors = FALSE
)

rec <- recipe(~ ., data = ex_dat)

test_that('basic functionality', {
  ex_1 <- rec %>%
    step_factor2string(y, z) %>%
    prep(ex_dat, strings_as_factors = FALSE) %>%
    juice
  expect_equal(class(ex_1$w), "character")
  expect_equal(class(ex_1$x), "character")
})

test_that('bad args', {
  expect_error(
    rec %>%
      step_factor2string(w, x) %>%
      prep(ex_dat, strings_as_factors = FALSE)
  )

})


test_that('printing', {
  ex_3 <- rec %>%
    step_factor2string(y, z) %>%
    prep(ex_dat, strings_as_factors = FALSE)
  expect_output(print(ex_3))
  expect_output(prep(ex_3, training = ex_dat, verbose = TRUE))
})


