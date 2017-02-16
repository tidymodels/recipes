library(testthat)
context("Testing center and scale")

library(magrittr)
library(recipes)

means <- vapply(biomass[, 3:7], mean, c(mean = 0))
sds <- vapply(biomass[, 3:7], sd, c(sd = 0))

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('correct means and std devs', {
  standardized <- rec %>% 
    step_center(~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>% 
    step_scale(~ carbon + hydrogen + oxygen + nitrogen + sulfur) 
  
  standardized_trained <- learn(standardized, training = biomass, verbose = FALSE)
  
  expect_equal(standardized_trained$steps[[1]]$means, means)
  expect_equal(standardized_trained$steps[[2]]$sds, sds)
})

test_that('training in stages', {
  at_once <- rec %>% 
    step_center(~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>% 
    step_scale(~ carbon + hydrogen + oxygen + nitrogen + sulfur) 
  
  at_once_trained <- learn(at_once, training = biomass, verbose = FALSE)
  
  ## not train in stages
  center_first <- rec %>% 
    step_center(~ carbon + hydrogen + oxygen + nitrogen + sulfur)
  center_first_trained <- learn(center_first, training = biomass, verbose = FALSE)
  in_stages <- center_first_trained %>%
    step_scale(~ carbon + hydrogen + oxygen + nitrogen + sulfur) 
  in_stages_trained <- learn(in_stages, training = biomass, verbose = FALSE)
  in_stages_retrained <- learn(in_stages, training = biomass, verbose = FALSE, fresh = TRUE) 
  
  expect_equal(at_once_trained, in_stages_trained)
  expect_equal(at_once_trained, in_stages_retrained)
  
})


test_that('single predictor', {
  standardized <- rec %>% 
    step_center(~ carbon) %>% 
    step_scale(~ hydrogen) 
  
  standardized_trained <- learn(standardized, training = biomass, verbose = FALSE)
  results <- process(standardized_trained, biomass)
  
  exp_res <- biomass[, 3:8]
  exp_res$carbon <- exp_res$carbon - mean(exp_res$carbon)
  exp_res$hydrogen <- exp_res$hydrogen / sd(exp_res$hydrogen)
  
  expect_equal(as.data.frame(results), exp_res[, colnames(results)])
})


