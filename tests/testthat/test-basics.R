library(testthat)
context("Testing basic functionalities")
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
  
  centered_char <- prep(centered, training = biomass, stringsAsFactors = FALSE, retain = TRUE)
  char_var <- bake(centered_char, newdata = head(biomass))
  expect_equal(class(char_var$sample), "character")
  
  centered_fac <- prep(centered, training = biomass, stringsAsFactors = TRUE, retain = TRUE)
  fac_var <- bake(centered_fac, newdata = head(biomass))
  expect_equal(class(fac_var$sample), "factor")  
  expect_equal(levels(fac_var$sample), sort(unique(biomass$sample)))  
})


test_that("Using prepare", {
  expect_error(prepare(recipe(HHV ~ ., data = biomass), 
                       training = biomass),
               paste0("As of version 0.0.1.9006, used `prep` ",
                      "instead of `prepare`"))
})

test_that("Multiple variables on lhs of formula", {
  # from issue #96
  expect_silent(multi_1 <- recipe(Petal.Width + Species ~ ., data = iris))
  expect_equal(multi_1$var_info$variable[multi_1$var_info$role == "outcome"],
               names(iris)[4:5])
  expect_equal(multi_1$var_info$variable[multi_1$var_info$role == "predictor"],
               names(iris)[1:3]) 
  
  iris$Species <- as.character(iris$Species)
  expect_silent(multi_2 <- recipe(Petal.Width + Species ~ ., data = iris))
  expect_equal(multi_2$var_info$variable[multi_2$var_info$role == "outcome"],
               names(iris)[4:5])
  expect_equal(multi_2$var_info$variable[multi_2$var_info$role == "predictor"],
               names(iris)[1:3]) 
    
})


