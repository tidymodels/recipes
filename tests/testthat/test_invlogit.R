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
    step_invlogit(x1)
  
  rec_trained <- learn(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- process(rec_trained, newdata = ex_dat)
  
  exp_res <- as_tibble(ex_dat)
  exp_res$x1 <- binomial()$linkinv(exp_res$x1)
  expect_equal(rec_trans, exp_res)
})
