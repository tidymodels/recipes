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
    step_string2factor(w, x) %>%
    prep(ex_dat, stringsAsFactors = FALSE, retain = TRUE) %>%
    juice
  expect_equal(class(ex_1$w), "factor")
  expect_equal(class(ex_1$x), "factor")
  expect_equal(levels(ex_1$w), letters[1:3])
  expect_equal(levels(ex_1$x), LETTERS[1:2])

  ex_2 <- rec %>%
    step_string2factor(w, x, ordered = TRUE) %>%
    prep(ex_dat, stringsAsFactors = FALSE, retain = TRUE) %>%
    juice
  expect_equal(class(ex_2$w), c("ordered", "factor"))
  expect_equal(class(ex_2$x), c("ordered", "factor"))
  expect_equal(levels(ex_2$w), letters[1:3])
  expect_equal(levels(ex_2$x), LETTERS[1:2])
})

test_that('bad args', {
  expect_error(
  rec %>%
    step_string2factor(w, x) %>%
    prep(ex_dat)
  )
  expect_error(
    rec %>%
      step_string2factor(y, z) %>%
      prep(ex_dat)
  )
  expect_error(
    rec %>%
      step_string2factor(y, z, ordered = "yes") %>%
      prep(ex_dat)
  )
  expect_error(
    rec %>%
      step_string2factor(y, z, levels = list("a","b")) %>%
      prep(ex_dat)
  )
})


test_that('printing', {
  ex_3 <- rec %>%
    step_string2factor(w, x) %>%
    prep(ex_dat, stringsAsFactors = FALSE, retain = TRUE)
  expect_output(print(ex_3))
  expect_output(prep(ex_3, training = ex_dat, verbose = TRUE))
})


