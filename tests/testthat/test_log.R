library(testthat)
library(recipes)
library(tibble)

n <- 20
set.seed(1)
ex_dat <- data.frame(x1 = exp(rnorm(n, mean = .1)),
                     x2 = 1/abs(rnorm(n)),
                     x3 = rep(1:2, each = n/2),
                     x4 = rexp(n))

test_that('simple log trans', {
  rec <- recipe(~., data = ex_dat) %>%
    step_log(x1, x2, x3, x4)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, newdata = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat, log))

  expect_equal(rec_trans, exp_res)
})


test_that('alt base', {
  rec <- recipe(~., data = ex_dat) %>%
    step_log(x1, x2, x3, x4, base = pi)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, newdata = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat, log, base = pi))

  expect_equal(rec_trans, exp_res)
})


test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>%
    step_log(x1, x2, x3, x4)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})

