library(testthat)
library(magrittr)
library(recipes)

n <- 100
set.seed(424)
dat <- matrix(rnorm(n*5), ncol =  5)
dat <- as.data.frame(dat)
dat$duplicate <- dat$V1
dat$V6 <- -dat$V2 + runif(n)*.2

test_that('high filter', {
  rec <- recipe(~ ., data = dat)
  filtering <- rec %>% 
    step_corr(~ is_predictor(), threshold = .5)
  
  filtering_trained <- learn(filtering, training = dat, verbose = FALSE)
  
  removed <- paste0("V", 1:2)
  
  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that('low filter', {
  rec <- recipe(~ ., data = dat)
  filtering <- rec %>% 
    step_corr(~ is_predictor(), threshold = 1)
  
  filtering_trained <- learn(filtering, training = dat, verbose = FALSE)

  expect_equal(filtering_trained$steps[[1]]$removals, numeric(0))
})
