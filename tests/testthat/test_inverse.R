library(testthat)
library(recipes)
library(tibble)

context("inverse trans")


n <- 20
set.seed(1)
ex_dat <- data.frame(x1 = exp(rnorm(n, mean = .1)),
                     x2 = 1/abs(rnorm(n)),
                     x3 = rep(1:2, each = n/2),
                     x4 = rexp(n),
                     x5 = rep(0:1, each = n/2))

test_that('simple inverse trans', {
  rec <- recipe( ~ x1 + x2 + x3 + x4, data = ex_dat) %>%
    step_inverse(x1, x2, x3, x4)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat[,-5], function(x) 1 / x))

  expect_equal(rec_trans, exp_res)
})

test_that('alt offset', {
  rec <- recipe(~., data = ex_dat) %>%
    step_inverse(x1, x2, x3, x4, x5, offset = 0.1)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat, function(x) 1/(x + 0.1)))

  expect_equal(rec_trans, exp_res)
})

test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>%
    step_inverse(x1, x2, x3, x4)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})

