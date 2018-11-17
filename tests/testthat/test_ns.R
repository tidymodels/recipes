library(testthat)
library(recipes)
data(biomass)
library(splines)

context("Natural splines")

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

test_that('correct basis functions', {
  with_ns <- rec %>%
    step_ns(carbon, hydrogen)

  with_ns <- prep(with_ns, training = biomass_tr, verbose = FALSE)

  with_ns_pred_tr <- bake(with_ns, new_data = biomass_tr)
  with_ns_pred_te <- bake(with_ns, new_data = biomass_te)

  carbon_ns_tr_exp <- ns(biomass_tr$carbon, df = 2)
  hydrogen_ns_tr_exp <- ns(biomass_tr$hydrogen, df = 2)
  carbon_ns_te_exp <- predict(carbon_ns_tr_exp, biomass_te$carbon)
  hydrogen_ns_te_exp <- predict(hydrogen_ns_tr_exp, biomass_te$hydrogen)

  carbon_ns_tr_res <- as.matrix(with_ns_pred_tr[, grep("carbon", names(with_ns_pred_tr))])
  colnames(carbon_ns_tr_res) <- NULL
  hydrogen_ns_tr_res <- as.matrix(with_ns_pred_tr[, grep("hydrogen", names(with_ns_pred_tr))])
  colnames(hydrogen_ns_tr_res) <- NULL

  carbon_ns_te_res <- as.matrix(with_ns_pred_te[, grep("carbon", names(with_ns_pred_te))])
  colnames(carbon_ns_te_res) <- 1:ncol(carbon_ns_te_res)
  hydrogen_ns_te_res <- as.matrix(with_ns_pred_te[, grep("hydrogen", names(with_ns_pred_te))])
  colnames(hydrogen_ns_te_res) <- 1:ncol(hydrogen_ns_te_res)

  ## remove attributes
  carbon_ns_tr_exp <- matrix(carbon_ns_tr_exp, ncol = 2)
  carbon_ns_te_exp <- matrix(carbon_ns_te_exp, ncol = 2)
  hydrogen_ns_tr_exp <- matrix(hydrogen_ns_tr_exp, ncol = 2)
  hydrogen_ns_te_exp <- matrix(hydrogen_ns_te_exp, ncol = 2)
  dimnames(carbon_ns_tr_res) <- NULL
  dimnames(carbon_ns_te_res) <- NULL
  dimnames(hydrogen_ns_tr_res) <- NULL
  dimnames(hydrogen_ns_te_res) <- NULL

  expect_equal(carbon_ns_tr_res, carbon_ns_tr_exp)
  expect_equal(carbon_ns_te_res, carbon_ns_te_exp)
  expect_equal(hydrogen_ns_tr_res, hydrogen_ns_tr_exp)
  expect_equal(hydrogen_ns_te_res, hydrogen_ns_te_exp)
})


test_that('printing', {
  with_ns <- rec %>%  step_ns(carbon, hydrogen)
  expect_output(print(with_ns))
  expect_output(prep(with_ns, training = biomass_tr, verbose = TRUE))
})

