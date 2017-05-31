library(testthat)
library(magrittr)
library(gower)
library(recipes)
data("biomass")

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

# induce some missing data at random
set.seed(9039)
carb_missing <- sample(1:nrow(biomass_te), 3)
nitro_missing <- sample(1:nrow(biomass_te), 3)

biomass_te$carbon[carb_missing] <- NA
biomass_te$nitrogen[nitro_missing] <- NA

test_that('imputation models', {
  discr_rec <- rec %>%
    step_discretize(nitrogen, options = list(keep_na = FALSE))
  impute_rec <- discr_rec %>%
    step_knnimpute(carbon,
                   nitrogen,
                   impute_with = imp_vars(hydrogen, oxygen, nitrogen),
                   K = 3)

  discr_rec <- learn(discr_rec, training = biomass_tr, verbose = FALSE)
  discr_tr <- process(discr_rec, newdata = biomass_tr)
  discr_te <- process(discr_rec, newdata = biomass_te) %>%
    select(hydrogen, oxygen, nitrogen, carbon)
  
  gower_topn(discr_tr[, c("hydrogen", "oxygen", "nitrogen")],
             discr_te[, c("hydrogen", "oxygen", "nitrogen")])$index
  
  imputed_tr <- learn(impute_rec, training = biomass_tr, verbose = FALSE)
  imputed_te <- process(impute_rec, newdata = biomass_te)
  
  
  
  
  

})






