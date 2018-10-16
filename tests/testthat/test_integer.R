library(testthat)
library(recipes)

context("Integer conversion")

set.seed(364)
tr_n <- 10
tr_dat <- 
  data.frame(
    x = sample(letters[1:5], replace = TRUE, size = tr_n),
    y = factor(sample(LETTERS[1:5], replace = TRUE, size = tr_n)),
    z = ordered(sample(month.abb, replace = TRUE, size = tr_n), levels = month.abb)
  )
tr_dat$x[3] <- NA
tr_dat$y[1] <- NA

te_n <- 4
te_dat <- 
  data.frame(
    x = c(sample(letters[1:5], replace = TRUE, size = te_n), "?"),
    y = factor(c(sample(LETTERS[1:5], replace = TRUE, size = te_n), "??")),
    z = ordered(c(sample(month.abb, replace = TRUE, size = te_n), "???"), 
                levels = c(month.abb, "???"))
  )
te_dat$x[1] <- NA
te_dat$y[1] <- NA

test_that('basic functionality', {
  rec <- recipe( ~ x + y + z, data = tr_dat) %>%
    step_integer(all_predictors())
  rec_trained <- prep(rec, traning = tr_dat, retain = TRUE)
  
  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, te_dat, all_predictors())

  exp_x <- c(NA, 1, 2, 3, 0)
  exp_y <- c(NA, 3, 3, 3, 0)
  exp_z <- c(11, 1, 9, 6, 0)
  
  expect_equal(te_int$x, exp_x)
  expect_equal(te_int$y, exp_y)
  expect_equal(te_int$z, exp_z)
  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(all(!vapply(te_int, is.integer, logical(1))))
})


test_that('zero-based', {
  rec <- recipe( ~ x + y + z, data = tr_dat) %>%
    step_integer(all_predictors(), zero_based = TRUE)
  rec_trained <- prep(rec, traning = tr_dat, retain = TRUE)
  
  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, te_dat, all_predictors())
  
  exp_x <- c(NA, 0, 1, 2, 4)
  exp_y <- c(NA, 2, 2, 2, 5)
  exp_z <- c(10, 0, 8, 5, 12)
  
  expect_equal(te_int$x, exp_x)
  expect_equal(te_int$y, exp_y)
  expect_equal(te_int$z, exp_z)
  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(all(!vapply(te_int, is.integer, logical(1))))
})

test_that('integers', {
  rec <- recipe( ~ x + y + z, data = tr_dat) %>%
    step_integer(all_predictors(), strict = TRUE)
  rec_trained <- prep(rec, traning = tr_dat, retain = TRUE)
  
  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, te_dat, all_predictors())
  
  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(all(vapply(te_int, is.integer, logical(1))))
  expect_true(all(vapply(tr_int, is.numeric, logical(1))))
  expect_true(all(vapply(tr_int, is.integer, logical(1))))   

})

test_that('printing', {
  rec <- recipe( ~ x + y + z, data = tr_dat)
  ints <- rec %>% step_integer(all_predictors())
  expect_output(print(ints))
  expect_output(prep(ints, training = tr_dat, verbose = TRUE))
})

