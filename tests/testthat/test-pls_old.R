library(testthat)
library(recipes)

skip_if_not_installed("modeldata")

## -----------------------------------------------------------------------------

data(biomass, package = "modeldata")

biom_tr <- biomass |>
  dplyr::filter(dataset == "Training") |>
  dplyr::select(-dataset, -sample)
biom_te <- biomass |>
  dplyr::filter(dataset == "Testing") |>
  dplyr::select(-dataset, -sample, -HHV)

load(test_path("test_pls_old.RData"))

## -----------------------------------------------------------------------------

test_that("check old PLS scores from recipes version <= 0.1.12", {
  new_values_tr <- bake(old_pls, new_data = NULL, all_predictors())
  expect_equal(new_values_tr, old_pls_tr)

  # Capture known warning about `keep_original_cols`

  suppressWarnings(new_values_te <- bake(old_pls, biom_te))
  expect_equal(new_values_te, old_pls_te)
  expect_snapshot(new_values_te <- bake(old_pls, biom_te))
})
