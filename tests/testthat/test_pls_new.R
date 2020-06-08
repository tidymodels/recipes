context("PLS (new objects)")

library(testthat)
library(recipes)
library(dplyr)
library(modeldata)

## -----------------------------------------------------------------------------

data(biomass, package = "modeldata")

biom_tr <- biomass %>% dplyr::filter(dataset == "Training") %>% dplyr::select(-dataset, -sample)
biom_te <- biomass %>% dplyr::filter(dataset == "Testing")  %>% dplyr::select(-dataset, -sample, -HHV)

data(cells, package = "modeldata")

cell_tr <- cells %>% dplyr::filter(case == "Train") %>% dplyr::select(-case)
cell_te <- cells %>% dplyr::filter(case == "Test")  %>% dplyr::select(-case, -class)

load(test_path("test_pls_new.RData"))


## -----------------------------------------------------------------------------

test_that('PLS, dense loadings', {
  rec <- recipe(HHV ~ ., data = biom_tr) %>%
    step_pls(all_predictors(), outcome = "HHV", num_comp = 3)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- juice(rec, all_predictors())
  expect_equal(tr_new, bm_pls_tr)
  te_new <- bake(rec, biom_te)
  expect_equal(te_new, bm_pls_te)
})


test_that('PLS, sparse loadings', {
  rec <- recipe(HHV ~ ., data = biom_tr) %>%
    step_pls(all_predictors(), outcome = "HHV", num_comp = 3, num_terms = 3)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- juice(rec, all_predictors())
  expect_equal(tr_new, bm_spls_tr)
  te_new <- bake(rec, biom_te)
  expect_equal(te_new, bm_spls_te)
})

## -----------------------------------------------------------------------------

test_that('PLS-DA, dense loadings', {
  rec <- recipe(class ~ ., data = cell_tr) %>%
    step_pls(all_predictors(), outcome = "class", num_comp = 3)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- juice(rec, all_predictors())
  expect_equal(tr_new, cell_plsda_tr)
  te_new <- bake(rec, cell_te)
  expect_equal(te_new, cell_plsda_te)
})


test_that('PLS-DA, sparse loadings', {
  rec <- recipe(class ~ ., data = cell_tr) %>%
    step_pls(all_predictors(), outcome = "class", num_comp = 3, num_terms = 50)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- juice(rec, all_predictors())
  expect_equal(tr_new, cell_splsda_tr)
  te_new <- bake(rec, cell_te)
  expect_equal(te_new, cell_splsda_te)
})
