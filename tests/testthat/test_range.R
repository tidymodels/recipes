library(testthat)
library(recipes)
data(biomass)

biomass_tr <- biomass[1:10,]
biomass_te <- biomass[c(13:14, 19, 522),]

rec <- recipe(HHV ~ carbon + hydrogen,
              data = biomass_tr)

test_that('correct values', {
  standardized <- rec %>% 
    step_range(carbon, hydrogen, min = -12) 
  
  standardized_trained <- prep(standardized, training = biomass_tr, verbose = FALSE)
  
  obs_pred <- bake(standardized_trained, newdata = biomass_te)
  obs_pred <- as.matrix(obs_pred)
  
  mins <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, min)
  maxs <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, max)  
  
  new_min <- -12
  new_max <- 1
  new_range <- new_max - new_min
  
  carb <- ((new_range * (biomass_te$carbon - mins["carbon"])) / 
           (maxs["carbon"] - mins["carbon"])) + new_min
  carb <- ifelse(carb > new_max, new_max, carb)
  carb <- ifelse(carb < new_min, new_min, carb)  
  
  hydro <- ((new_range * (biomass_te$hydrogen - mins["hydrogen"])) / 
              (maxs["hydrogen"] - mins["hydrogen"])) + new_min
  hydro <- ifelse(hydro > new_max, new_max, hydro)
  hydro <- ifelse(hydro < new_min, new_min, hydro)  
  
  exp_pred <- cbind(carb, hydro)
  colnames(exp_pred) <- c("carbon", "hydrogen")
  expect_equal(exp_pred, obs_pred)
})


test_that('defaults', {
  standardized <- rec %>% 
    step_range(carbon, hydrogen) 
  
  standardized_trained <- prep(standardized, training = biomass_tr, verbose = FALSE)
  
  obs_pred <- bake(standardized_trained, newdata = biomass_te)
  obs_pred <- as.matrix(obs_pred)
  
  mins <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, min)
  maxs <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, max)  
  
  new_min <- 0
  new_max <- 1
  new_range <- new_max - new_min
  
  carb <- ((new_range * (biomass_te$carbon - mins["carbon"])) / 
             (maxs["carbon"] - mins["carbon"])) + new_min
  carb <- ifelse(carb > new_max, new_max, carb)
  carb <- ifelse(carb < new_min, new_min, carb)  
  
  hydro <- ((new_range * (biomass_te$hydrogen - mins["hydrogen"])) / 
              (maxs["hydrogen"] - mins["hydrogen"])) + new_min
  hydro <- ifelse(hydro > new_max, new_max, hydro)
  hydro <- ifelse(hydro < new_min, new_min, hydro)  
  
  exp_pred <- cbind(carb, hydro)
  colnames(exp_pred) <- c("carbon", "hydrogen")
  expect_equal(exp_pred, obs_pred)
})


test_that('one variable', {
  standardized <- rec %>% 
    step_range(carbon) 
  
  standardized_trained <- prep(standardized, training = biomass_tr, verbose = FALSE)
  
  obs_pred <- bake(standardized_trained, newdata = biomass_te)

  mins <- min(biomass_tr$carbon)
  maxs <- max(biomass_tr$carbon)
  
  new_min <- 0
  new_max <- 1
  new_range <- new_max - new_min
  
  carb <- ((new_range * (biomass_te$carbon - mins)) / 
             (maxs - mins)) + new_min
  carb <- ifelse(carb > new_max, new_max, carb)
  carb <- ifelse(carb < new_min, new_min, carb)  

  expect_equal(carb, obs_pred$carbon)
})


test_that('printing', {
  standardized <- rec %>% 
    step_range(carbon, hydrogen, min = -12) 
  expect_output(print(standardized))
  expect_output(prep(standardized, training = biomass_tr))
})

