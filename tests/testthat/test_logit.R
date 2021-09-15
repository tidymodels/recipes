library(testthat)
library(recipes)
library(dplyr)

n <- 20
set.seed(12)
ex_dat <- data.frame(x1 = runif(n),
                     x2 = rnorm(n),
                     x3 = seq(0, 1, length.out = 20))

test_that('simple logit trans', {
  rec <- recipe(~., data = ex_dat) %>%
    step_logit(x1)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(ex_dat)
  exp_res$x1 <- binomial()$linkfun(exp_res$x1)
  expect_equal(rec_trans, exp_res)

  rec <- recipe(~., data = ex_dat) %>%
    step_logit(x3, offset = 0.1)
  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)
  exp_res <-
    as_tibble(ex_dat) %>%
    mutate(
      x3 = case_when(
        x3 == 1.0 ~ 1 - 0.1,
        x3 == 0.0 ~ 0.1,
        TRUE ~ x3
      )
    )
  exp_res$x3 <- binomial()$linkfun(exp_res$x3)
  expect_equal(rec_trans, exp_res)

})


test_that('out of bounds logit trans', {
  rec <- recipe(~., data = ex_dat) %>%
    step_logit(x1, x2)

  expect_error(prep(rec, training = ex_dat, verbose = FALSE))
})


test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>%
    step_logit(x1)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})
