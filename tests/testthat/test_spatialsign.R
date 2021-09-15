library(testthat)
library(recipes)
library(modeldata)
data(biomass)

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('spatial sign', {
  sp_sign <- rec %>%
    step_center(carbon, hydrogen) %>%
    step_scale(carbon, hydrogen) %>%
    step_spatialsign(carbon, hydrogen)

  sp_sign_trained <- prep(sp_sign, training = biomass, verbose = FALSE)

  sp_sign_pred <- bake(sp_sign_trained, new_data = biomass)
  sp_sign_pred <- as.matrix(sp_sign_pred)[, c("carbon", "hydrogen")]

  x <- as.matrix(scale(biomass[, 3:4], center = TRUE, scale = TRUE))
  x <- t(apply(x, 1, function(x) x/sqrt(sum(x^2))))

  expect_equal(sp_sign_pred, x)
})

test_that('Missing values', {
  sp_sign <- rec %>%
    step_spatialsign(carbon, hydrogen)

  sp_sign_trained <- prep(sp_sign, training = biomass, verbose = FALSE)

  with_na <- head(biomass)
  with_na$carbon[1] <- NA
  with_na$hydrogen[2] <- NA
  rownames(with_na) <- NULL

  sp_sign_pred <- bake(sp_sign_trained, new_data = with_na)
  sp_sign_pred <- as.matrix(sp_sign_pred)[, c("carbon", "hydrogen")]

  x <- as.matrix(with_na[, 3:4])
  x <- t(apply(x, 1, function(x) x/sqrt(sum(x^2, na.rm = TRUE))))

  expect_equal(sp_sign_pred, x)
})


test_that('printing', {
  sp_sign <- rec %>%
    step_center(carbon, hydrogen) %>%
    step_scale(carbon, hydrogen) %>%
    step_spatialsign(carbon, hydrogen)
  expect_output(print(sp_sign))
  expect_output(prep(sp_sign, training = biomass, verbose = TRUE))
})


