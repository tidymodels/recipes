library(testthat)
library(recipes)
library(tibble)


context("Unordering data")

lmh <- c("Low", "Med", "High")

examples <- data.frame(X1 = factor(rep(letters[1:4], each = 3)),
                       X2 = ordered(rep(lmh, each = 4),
                                    levels = lmh))
rec <- recipe(~ X1 + X2, data = examples)

test_that('correct var', {
  rec1 <- rec %>% step_unorder(X2)

  rec1_trained <- prep(rec1, training = examples, verbose = FALSE)
  rec1_trans <- bake(rec1_trained, new_data = examples)

  expect_true(is.factor(rec1_trans$X2))
  expect_true(!is.ordered(rec1_trans$X2))

  expect_equal(levels(rec1_trans$X2), levels(examples$X2))
  expect_equal(as.character(rec1_trans$X2), as.character(examples$X2))
})

test_that('wrong vars', {
  rec2 <- rec %>% step_unorder(X1, X2)
  expect_warning(prep(rec2, training = examples, verbose = FALSE))
  rec3 <- rec %>% step_unorder(X1)
  expect_error(prep(rec3, training = examples, verbose = FALSE))
})

test_that('printing', {
  rec4 <- rec %>% step_unorder(X2)
  expect_output(print(rec4))
  expect_output(prep(rec4, training = examples, verbose = TRUE))
})

