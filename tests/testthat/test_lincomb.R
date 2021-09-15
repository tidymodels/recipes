library(testthat)
library(recipes)

dummies <- cbind(model.matrix( ~ block - 1, npk),
                 model.matrix( ~ N - 1, npk),
                 model.matrix( ~ P - 1, npk),
                 model.matrix( ~ K - 1, npk),
                 yield = npk$yield)

dummies <- as.data.frame(dummies)

dum_rec <- recipe(yield ~ . , data = dummies)

###################################################################

library(modeldata)
data(biomass)
biomass$new_1 <- with(biomass,
                      .1*carbon - .2*hydrogen + .6*sulfur)
biomass$new_2 <- with(biomass,
                      .5*carbon - .2*oxygen + .6*nitrogen)

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

biomass_rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
                        sulfur + new_1 + new_2,
                      data = biomass_tr)

###################################################################

test_that('example 1', {
  dum_filtered <- dum_rec %>%
    step_lincomb(all_predictors())
  dum_filtered <- prep(dum_filtered, training = dummies, verbose = FALSE)
  removed <- c("N1", "P1", "K1")
  expect_equal(dum_filtered$steps[[1]]$removals, removed)
})

test_that('example 2', {
  lincomb_filter <- biomass_rec %>%
    step_lincomb(all_predictors())

  filtering_trained <- prep(lincomb_filter, training = biomass_tr)
  test_res <- bake(filtering_trained, new_data = biomass_te, all_predictors())

  expect_true(all(!(paste0("new_", 1:2) %in% colnames(test_res))))
})

test_that('no exclusions', {
  biomass_rec_2 <- recipe(HHV ~ carbon + hydrogen, data = biomass_tr)
  lincomb_filter_2 <- biomass_rec_2 %>%
    step_lincomb(all_predictors())

  filtering_trained_2 <- prep(lincomb_filter_2, training = biomass_tr)
  test_res_2 <- bake(filtering_trained_2, new_data = biomass_te, all_predictors())

  expect_true(length(filtering_trained_2$steps[[1]]$removals) == 0)
  expect_true(all(colnames(test_res_2) == c("carbon", "hydrogen")))
})


test_that('printing', {
  dum_filtered <- dum_rec %>%
    step_lincomb(all_predictors())
  expect_output(print(dum_filtered))
  expect_output(prep(dum_filtered, training = dummies, verbose = TRUE))
})
