library(testthat)
library(recipes)
data("biomass")

tr_biomass <- subset(biomass, dataset == "Training")[, -(1:2)]
te_biomass <- subset(biomass, dataset == "Testing")[, -(1:2)]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = tr_biomass)

test_that('non-factor variables with dot', {
  int_rec <- rec %>% step_interact(~(.-HHV)^3, sep=":")
  int_rec_trained <- prep(int_rec, training = tr_biomass, verbose = FALSE)
  
  te_new <- bake(int_rec_trained, newdata = te_biomass, all_predictors())
  te_new <- te_new[, sort(names(te_new))]
  te_new <- as.matrix(te_new)
  
  og_terms <- terms(~(.-HHV)^3, data = te_biomass)
  te_og <- model.matrix(og_terms, data = te_biomass)[, -1]
  te_og <- te_og[, sort(colnames(te_og))]
  
  rownames(te_new) <- NULL
  rownames(te_og) <- NULL
  
  expect_equal(te_og, te_new)
})


test_that('non-factor variables with specific variables', {
  int_rec <- rec %>% step_interact(~carbon:hydrogen + oxygen:nitrogen:sulfur, sep = ":")
  int_rec_trained <- prep(int_rec, training = tr_biomass, verbose = FALSE)
  
  te_new <- bake(int_rec_trained, newdata = te_biomass, all_predictors())
  te_new <- te_new[, sort(names(te_new))]
  te_new <- as.matrix(te_new)
  
  og_terms <- terms(~carbon + hydrogen + oxygen + nitrogen + sulfur + 
                      carbon:hydrogen + oxygen:nitrogen:sulfur, data = te_biomass)
  te_og <- model.matrix(og_terms, data = te_biomass)[, -1]
  te_og <- te_og[, sort(colnames(te_og))]
  
  rownames(te_new) <- NULL
  rownames(te_og) <- NULL
  
  expect_equal(te_og, te_new)
})


test_that('printing', {
  int_rec <- rec %>% step_interact(~carbon:hydrogen)
  expect_output(print(int_rec))
  expect_output(prep(int_rec, training = tr_biomass))
})


# currently failing; try to figure out why
# test_that('with factors', {
#   int_rec <- recipe(Sepal.Width ~ ., data = iris) %>% 
#     step_interact(~ (. - Sepal.Width)^3, sep = ":")
#   int_rec_trained <- prep(int_rec, iris)
#   
#   te_new <- bake(int_rec_trained, newdata = iris, role = "predictor")
#   te_new <- te_new[, sort(names(te_new))]
#   te_new <- as.matrix(te_new)
#   
#   og_terms <- terms(Sepal.Width ~ (.)^3, data = iris)
#   te_og <- model.matrix(og_terms, data = iris)[, -1]
#   te_og <- te_og[, sort(colnames(te_og))]
#   
#   rownames(te_new) <- NULL
#   rownames(te_og) <- NULL
#   
#   all.equal(te_og, te_new)
# })

