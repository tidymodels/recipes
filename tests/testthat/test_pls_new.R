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
  skip_if_not_installed("mixOmics")
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
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) %>%
    step_pls(all_predictors(), outcome = "HHV", num_comp = 3, predictor_prop = 3/5)

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
  skip_if_not_installed("mixOmics")
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
  skip_if_not_installed("mixOmics")
  rec <- recipe(class ~ ., data = cell_tr) %>%
    step_pls(all_predictors(), outcome = "class", num_comp = 3, predictor_prop = 50/56)

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

## -----------------------------------------------------------------------------

test_that('No PLS', {
  skip_if_not_installed("mixOmics")
  rec <- recipe(class ~ ., data = cell_tr) %>%
    step_pls(all_predictors(), outcome = "class", num_comp = 0)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("x_vars", "y_vars")
  )
  pred_names <- summary(rec)$variable[summary(rec)$role == "predictor"]

  tr_new <- juice(rec, all_predictors())
  expect_equal(names(tr_new), pred_names)
  te_new <- bake(rec, cell_te, all_predictors())
  expect_equal(names(te_new), pred_names)
})

## -----------------------------------------------------------------------------

test_that('tidy method', {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) %>%
    step_pls(all_predictors(), outcome = "HHV", num_comp = 3, id = "dork")

  tidy_pre <- tidy(rec, number = 1)
  exp_pre <- tibble::tribble(
    ~terms, ~value, ~component,    ~id,
    "all_predictors()",     NA_real_,         NA_character_, "dork"
  )
  expect_equal(tidy_pre, exp_pre)

  rec <- prep(rec)
  tidy_post <- tidy(rec, number = 1)
  exp_post <-
    tibble::tribble(
      ~terms,             ~value, ~component,    ~id,
      "carbon",    0.82813459059393,      "PLS1", "dork",
      "carbon",    0.718469477422311,     "PLS2", "dork",
      "carbon",    0.476111929729498,     "PLS3", "dork",
      "hydrogen", -0.206963356355556,     "PLS1", "dork",
      "hydrogen",  0.642998926998282,     "PLS2", "dork",
      "hydrogen",  0.262836631090453,     "PLS3", "dork",
      "oxygen",   -0.49241242430895,      "PLS1", "dork",
      "oxygen",    0.299176769170812,     "PLS2", "dork",
      "oxygen",    0.418081563632953,     "PLS3", "dork",
      "nitrogen", -0.122633995804743,     "PLS1", "dork",
      "nitrogen", -0.172719084680244,     "PLS2", "dork",
      "nitrogen",  0.642403301090588,     "PLS3", "dork",
      "sulfur",    0.11768677260853,      "PLS1", "dork",
      "sulfur",   -0.217341766567037,     "PLS2", "dork",
      "sulfur",    0.521114256955661,     "PLS3", "dork"
    )
  expect_equal(tidy_post, exp_post, tolerance = 0.01)
})

## -----------------------------------------------------------------------------

test_that('print method', {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) %>%
    step_pls(all_predictors(), outcome = "HHV", num_comp = 3, id = "dork")

  expect_output(print(rec), "feature extraction with all_predictors")

  rec <- prep(rec)
  expect_output(
    print(rec),
    "feature extraction with carbon, hydrogen, oxygen, nitrogen, sulfur"
  )

})

test_that('keep_original_cols works', {

  skip_if_not_installed("mixOmics")
  pls_rec <- recipe(HHV ~ ., data = biom_tr) %>%
    step_pls(all_predictors(), outcome = "HHV", num_comp = 3, keep_original_cols = TRUE)

  pls_trained <- prep(pls_rec)
  pls_pred <- bake(pls_trained, new_data = biom_te, all_predictors())

  expect_equal(
    colnames(pls_pred),
    c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur",
      "PLS1", "PLS2", "PLS3")
  )
})

test_that('can prep recipes with no keep_original_cols', {
  skip_if_not_installed("mixOmics")
  pls_rec <- recipe(HHV ~ ., data = biom_tr) %>%
    step_pls(all_predictors(), outcome = "HHV", num_comp = 3)

  pls_rec$steps[[1]]$keep_original_cols <- NULL

  expect_warning(
    pls_trained <- prep(pls_rec, training = biom_tr, verbose = FALSE),
    "'keep_original_cols' was added to"
  )

  expect_error(
    pls_pred <- bake(pls_trained, new_data = biom_te, all_predictors()),
    NA
  )

})
