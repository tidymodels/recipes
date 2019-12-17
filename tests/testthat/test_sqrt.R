library(testthat)
library(recipes)
library(tibble)

context("sqrt transfomration")


n <- 20
ex_dat <- data.frame(x1 = seq(0, 1, length = n),
                     x2 = rep(1:5, 4))

test_that('simple sqrt trans', {

      rec <- recipe(~., data = ex_dat) %>%
        step_sqrt(x1, x2)

      rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
      rec_trans <- bake(rec_trained, new_data = ex_dat)

      exp_res <- as_tibble(lapply(ex_dat, sqrt))
      expect_equal(rec_trans, exp_res)

})


test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>%
    step_sqrt(x1, x2)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})

test_that("works with integer64", {
  df <- data.frame(x = bit64::as.integer64(1:5))

  rec <- recipe(~ x, df)
  rec <- step_sqrt(rec, x)

  prepped <- prep(rec, df)

  expect <- tibble::tibble(x = sqrt(as.double(df$x)))

  expect_equal(
    juice(prepped),
    expect
  )
})

