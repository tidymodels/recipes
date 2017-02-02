context("Testing basic functionalities")

library(tibble)
data("biomass")

test_that("Recipe correctly identifies output variable", {
  raw_recipe <- recipe(HHV ~ ., data = biomass)
  var_info <- raw_recipe$var_info
  expect_true(is.tibble(var_info))
  outcome_ind <- which(var_info$variable == "HHV")
  expect_true(var_info$role[outcome_ind] == "outcome")
  expect_true(all(var_info$role[-outcome_ind] == rep("predictor", ncol(biomass) - 1)))
})
