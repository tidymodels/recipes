library(testthat)
library(recipes)
library(tibble)

n <- 20
set.seed(12)
ex_dat <- data.frame(x1 = rnorm(n),
                     x2 = runif(n))

test_that('simple logit trans', {
  rec <- recipe(~., data = ex_dat) %>% 
    step_rm(x1)
  
  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_rm <- bake(rec_trained, newdata = ex_dat)
  
  expect_equal(colnames(rec_rm), "x2")
})

test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>% 
    step_rm(x1)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat))
})


test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>% 
    step_rm(x1)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat))
})

