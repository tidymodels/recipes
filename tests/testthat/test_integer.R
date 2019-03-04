library(testthat)
library(recipes)

context("Integer conversion")

tr_n <- 10
tr_dat <-
  data.frame(
    x = c('a', 'd', 'c', 'a', 'c', 'a', 'a', 'd', 'c', 'c'),
    y = factor(c('B', 'B', 'D', 'B', 'C', 'D', 'A', 'D', 'C', 'C')),
    z = ordered(c('Jul', 'Apr', 'Sep', 'Jul', 'Nov', 'Dec', 'Jun', 'Feb', 'Jan', 'Sep'),
                levels = month.abb)
  )
tr_dat$x[3] <- NA
tr_dat$y[1] <- NA

te_n <- 4
te_dat <-
  data.frame(
    x = c('d', 'c', 'c', 'a', "?"),
    y = factor(c('E', 'D', 'C', 'C', "??")),
    z = ordered(c('Feb', 'Aug', 'Dec', 'Aug', "???"), levels = c(month.abb, "???"))
  )
te_dat$x[1] <- NA
te_dat$y[1] <- NA

test_that('basic functionality', {
  rec <- recipe( ~ x + y + z, data = tr_dat) %>%
    step_integer(all_predictors())
  rec_trained <- prep(rec, traning = tr_dat, retain = TRUE)

  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, te_dat, all_predictors())

  exp_x <- c(NA, 2, 2, 1, 0)
  exp_y <- c(NA, 4, 3, 3, 0)
  exp_z <- c( 2, 8,12, 8, 0)

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

  exp_x <- c(NA, 1, 1, 0, 3)
  exp_y <- c(NA, 3, 2, 2, 4)
  exp_z <- c( 1, 7,11, 7, 12)

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


