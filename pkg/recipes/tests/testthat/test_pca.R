library(testthat)
library(magrittr)
library(recipes)
data(biomass)

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('correct PCA values', {
  pca_extract <- rec %>% 
    step_center(~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>% 
    step_scale(~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>%
    step_pca(~ carbon + hydrogen + oxygen + nitrogen + sulfur)
  
  pca_extract_trained <- learn(pca_extract, training = biomass, verbose = FALSE)
  
  pca_pred <- process(pca_extract_trained, newdata = biomass)
  pca_pred <- as.matrix(pca_pred)[, -1]
  
  pca_obj <- prcomp(biomass[, 3:7], center = TRUE, scale. = TRUE, retx = TRUE)
  
  expect_equal(pca_extract_trained$steps[[3]]$object$rotation, pca_obj$rotation)
  expect_equal(pca_pred, pca_obj$x)
})


test_that('Reduced rotation size', {
  pca_extract <- rec %>% 
    step_center(~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>% 
    step_scale(~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>%
    step_pca(~ carbon + hydrogen + oxygen + nitrogen + sulfur, num = 3)
  
  pca_extract_trained <- learn(pca_extract, training = biomass, verbose = FALSE)
  
  pca_pred <- process(pca_extract_trained, newdata = biomass)
  pca_pred <- as.matrix(pca_pred[, paste0("PC", 1:3)])
  
  pca_obj <- prcomp(biomass[, 3:7], center = TRUE, scale. = TRUE)
  pca_obj$rotation <- pca_obj$rotation[, 1:3]
  pca_obj_pred <- predict(pca_obj, biomass[, 3:7])
  
  expect_equal(pca_pred, pca_obj_pred)
})
