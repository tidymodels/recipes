library(testthat)
library(ipred)
library(rpart)
library(recipes)

context("bagged imputation")

library(modeldata)
data(biomass)

biomass$fac <- factor(sample(letters[1:2], size = nrow(biomass), replace = TRUE))

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + fac,
              data = biomass)

test_that('imputation models', {
  imputed <- rec %>%
    step_bagimpute(carbon, fac, impute_with = imp_vars(hydrogen, oxygen),
                   seed_val = 12, trees = 5)

  imputed_trained <- prep(imputed, training = biomass, verbose = FALSE)

  expect_equal(length(imputed_trained$steps[[1]]$models[["carbon"]]$mtrees), 5)

  ## make sure we get the same trees given the same random samples
  carb_samps <- lapply(imputed_trained$steps[[1]]$models[["carbon"]]$mtrees,
                       function(x) x$bindx)
  for (i in seq_along(carb_samps)) {
    carb_data <- biomass[carb_samps[[i]], c("carbon", "hydrogen", "oxygen")]
    carb_mod <- rpart(carbon ~ ., data = carb_data,
                      control = rpart.control(xval = 0))
    expect_equal(carb_mod$splits,
                 imputed_trained$steps[[1]]$models[["carbon"]]$mtrees[[i]]$btree$splits)

  }

  fac_samps <- lapply(imputed_trained$steps[[1]]$models[[1]]$mtrees,
                      function(x) x$bindx)

  fac_ctrl <- imputed_trained$steps[[1]]$models[["fac"]]$mtrees[[1]]$btree$control

  ## make sure we get the same trees given the same random samples
  for (i in seq_along(fac_samps)) {
    fac_data <- biomass[fac_samps[[i]], c("fac", "hydrogen", "oxygen")]
    fac_mod <- rpart(fac ~ ., data = fac_data, control = fac_ctrl)
    expect_equal(fac_mod$splits,
                 imputed_trained$steps[[1]]$models[["fac"]]$mtrees[[i]]$btree$splits)
  }

  imp_tibble_un <-
    tibble(terms = c("carbon", "fac"),
           model = rep(NA, 2),
           id = imputed_trained$steps[[1]]$id)
  imp_tibble_tr <-
    tibble(terms = c("carbon", "fac"),
           model = imputed_trained$steps[[1]]$models,
           id = imputed_trained$steps[[1]]$id)

  expect_equivalent(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_un))
  expect_equal(tidy(imputed_trained, 1)$terms, imp_tibble_tr$terms)
  expect_equal(tidy(imputed_trained, 1)$model, imp_tibble_tr$model)


})


test_that('printing', {
  imputed <- rec %>%
    step_bagimpute(carbon, impute_with = imp_vars(hydrogen), seed_val = 12,
                   trees = 7)

  expect_output(print(imputed))
  expect_output(prep(imputed, training = biomass, verbose = TRUE))
})

test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_bagimpute(all_predictors(), impute_with = imp_vars(all_predictors()))
  rec_param <- tunable.step_bagimpute(rec$steps[[1]])
  expect_equal(rec_param$name, c("trees"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})

