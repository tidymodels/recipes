library(testthat)
context("Testing basic functionalities")
library(magrittr)
library(tibble)

library(recipes)
data("biomass")

test_that("Recipe correctly identifies output variable", {
  raw_recipe <- recipe(HHV ~ ., data = biomass)
  var_info <- raw_recipe$var_info
  expect_true(is.tibble(var_info))
  outcome_ind <- which(var_info$variable == "HHV")
  expect_true(var_info$role[outcome_ind] == "outcome")
  expect_true(all(var_info$role[-outcome_ind] == rep("predictor", ncol(biomass) - 1)))
})

test_that("Recipe fails on in-line functions", {
  expect_error(recipe(HHV ~ log(nitrogen), data = biomass))
  expect_error(recipe(HHV ~ (.)^2, data = biomass))
  expect_error(recipe(HHV ~ nitrogen  + sulfur  + nitrogen:sulfur, data = biomass))
  expect_error(recipe(HHV ~ nitrogen^2, data = biomass))
})

test_that("return character or factor values", {
  raw_recipe <- recipe(HHV ~ ., data = biomass)
  centered <- raw_recipe %>% 
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur)
  
  centered_char <- learn(centered, training = biomass, stringsAsFactors = FALSE, retain = TRUE)
  char_var <- process(centered_char, newdata = head(biomass))
  expect_equal(class(char_var$sample), "character")
  
  centered_fac <- learn(centered, training = biomass, stringsAsFactors = TRUE, retain = TRUE)
  fac_var <- process(centered_fac, newdata = head(biomass))
  expect_equal(class(fac_var$sample), "factor")  
  expect_equal(levels(fac_var$sample), sort(unique(biomass$sample)))  
})
