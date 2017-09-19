library(testthat)
library(recipes)
data("biomass")

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('spatial sign', {
  sp_sign <- rec %>%
    step_center(carbon, hydrogen) %>%
    step_scale(carbon, hydrogen) %>%
    step_spatialsign(carbon, hydrogen)

  sp_sign_trained <- prep(sp_sign, training = biomass, verbose = FALSE)

  sp_sign_pred <- bake(sp_sign_trained, newdata = biomass)
  sp_sign_pred <- as.matrix(sp_sign_pred)[, c("carbon", "hydrogen")]

  x <- as.matrix(scale(biomass[, 3:4], center = TRUE, scale = TRUE))
  x <- t(apply(x, 1, function(x) x/sqrt(sum(x^2))))

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


