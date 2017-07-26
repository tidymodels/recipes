library(testthat)
library(recipes)
data(biomass)

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

test_that('correct basis functions', {
  with_poly <- rec %>% 
    step_poly(carbon, hydrogen)
  
  with_poly <- prep(with_poly, training = biomass_tr, verbose = FALSE)
  
  with_poly_pred_tr <- bake(with_poly, newdata = biomass_tr)
  with_poly_pred_te <- bake(with_poly, newdata = biomass_te)
  
  carbon_poly_tr_exp <- poly(biomass_tr$carbon, degree = 2)
  hydrogen_poly_tr_exp <- poly(biomass_tr$hydrogen, degree = 2)
  carbon_poly_te_exp <- predict(carbon_poly_tr_exp, biomass_te$carbon)
  hydrogen_poly_te_exp <- predict(hydrogen_poly_tr_exp, biomass_te$hydrogen)
  
  carbon_poly_tr_res <- as.matrix(with_poly_pred_tr[, grep("carbon", names(with_poly_pred_tr))])
  colnames(carbon_poly_tr_res) <- NULL
  hydrogen_poly_tr_res <- as.matrix(with_poly_pred_tr[, grep("hydrogen", names(with_poly_pred_tr))])
  colnames(hydrogen_poly_tr_res) <- NULL
  
  carbon_poly_te_res <- as.matrix(with_poly_pred_te[, grep("carbon", names(with_poly_pred_te))])
  colnames(carbon_poly_te_res) <- 1:ncol(carbon_poly_te_res)
  hydrogen_poly_te_res <- as.matrix(with_poly_pred_te[, grep("hydrogen", names(with_poly_pred_te))])
  colnames(hydrogen_poly_te_res) <- 1:ncol(hydrogen_poly_te_res)
  
  ## remove attributes
  carbon_poly_tr_exp <- matrix(carbon_poly_tr_exp, ncol = 2)
  carbon_poly_te_exp <- matrix(carbon_poly_te_exp, ncol = 2)
  hydrogen_poly_tr_exp <- matrix(hydrogen_poly_tr_exp, ncol = 2)
  hydrogen_poly_te_exp <- matrix(hydrogen_poly_te_exp, ncol = 2) 
  dimnames(carbon_poly_tr_res) <- NULL
  dimnames(carbon_poly_te_res) <- NULL  
  dimnames(hydrogen_poly_tr_res) <- NULL
  dimnames(hydrogen_poly_te_res) <- NULL  
  
  expect_equal(carbon_poly_tr_res, carbon_poly_tr_exp)
  expect_equal(carbon_poly_te_res, carbon_poly_te_exp)
  expect_equal(hydrogen_poly_tr_res, hydrogen_poly_tr_exp)
  expect_equal(hydrogen_poly_te_res, hydrogen_poly_te_exp)  
})


test_that('printing', {
  with_poly <- rec %>% 
    step_poly(carbon, hydrogen)
  expect_output(print(with_poly))
  expect_output(prep(with_poly, training = biomass_tr))
})

