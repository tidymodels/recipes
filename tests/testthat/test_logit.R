library(testthat)
library(recipes)
library(tibble)

n <- 20
set.seed(12)
ex_dat <- data.frame(x1 = runif(n),
                     x2 = rnorm(n))

test_that('simple logit trans', {
  rec <- recipe(~., data = ex_dat) %>% 
    step_logit(x1)
  
  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, newdata = ex_dat)
  
  exp_res <- as_tibble(ex_dat)
  exp_res$x1 <- binomial()$linkfun(exp_res$x1)
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
  expect_output(prep(rec, training = ex_dat))
})
