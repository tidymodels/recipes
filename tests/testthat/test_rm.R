library(testthat)
library(magrittr)
library(recipes)
library(tibble)

n <- 20
set.seed(12)
ex_dat <- data.frame(x1 = rnorm(n),
                     x2 = runif(n))

test_that('simple logit trans', {
  rec <- recipe(~., data = ex_dat) %>% 
    step_rm(x1)
  
  rec_trained <- learn(rec, training = ex_dat, verbose = FALSE)
  rec_rm <- process(rec_trained, newdata = ex_dat)
  
  expect_equal(colnames(rec_rm), "x2")
})
