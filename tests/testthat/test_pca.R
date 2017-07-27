library(testthat)
library(recipes)
data(biomass)

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

test_that('correct PCA values', {
  pca_extract <- rec %>% 
    step_center(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>% 
    step_scale(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, 
             options = list(retx = TRUE))
  
  pca_extract_trained <- prep(pca_extract, training = biomass_tr, verbose = FALSE)
  
  pca_pred <- bake(pca_extract_trained, newdata = biomass_te)
  pca_pred <- as.matrix(pca_pred)
  
  pca_exp <- prcomp(biomass_tr[, 3:7], center = TRUE, scale. = TRUE, retx = TRUE)
  pca_pred_exp <- predict(pca_exp, biomass_te[, 3:7])[, 1:pca_extract$steps[[3]]$num]
  
  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL
  
  expect_equal(pca_pred, pca_pred_exp)
})

test_that('correct PCA values with threshold', {
  pca_extract <- rec %>% 
    step_center(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>% 
    step_scale(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, threshold = .5)
  
  pca_extract_trained <- prep(pca_extract, training = biomass_tr, verbose = FALSE)
  pca_exp <- prcomp(biomass_tr[, 3:7], center = TRUE, scale. = TRUE, retx = TRUE)
  # cumsum(pca_exp$sdev^2)/sum(pca_exp$sdev^2)

  expect_equal(pca_extract_trained$steps[[3]]$num, 2)
})


test_that('Reduced rotation size', {
  pca_extract <- rec %>% 
    step_center(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>% 
    step_scale(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, num = 3)
  
  pca_extract_trained <- prep(pca_extract, training = biomass_tr, verbose = FALSE)
  
  pca_pred <- bake(pca_extract_trained, newdata = biomass_te)
  pca_pred <- as.matrix(pca_pred)
  
  pca_exp <- prcomp(biomass_tr[, 3:7], center = TRUE, scale. = TRUE, retx = TRUE)
  pca_pred_exp <- predict(pca_exp, biomass_te[, 3:7])[, 1:3]
  rownames(pca_pred_exp) <- NULL
  
  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL
  
  expect_equal(pca_pred, pca_pred_exp)
})


test_that('printing', {
  pca_extract <- rec %>% 
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur)
  expect_output(print(pca_extract))
  expect_output(prep(pca_extract, training = biomass_tr))
})

