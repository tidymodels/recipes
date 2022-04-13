library(mixOmics)
library(dplyr)
library(modeldata)
library(stringr)

data(biomass, package = "modeldata")

biom_tr <- biomass %>%
  dplyr::filter(dataset == "Training") %>%
  dplyr::select(-dataset, -sample)
biom_te <- biomass %>%
  dplyr::filter(dataset == "Testing") %>%
  dplyr::select(-dataset, -sample, -HHV)

data(cells, package = "modeldata")

cell_tr <- cells %>%
  dplyr::filter(case == "Train") %>%
  dplyr::select(-case)
cell_te <- cells %>%
  dplyr::filter(case == "Test") %>%
  dplyr::select(-case, -class)

# pls 1 outcome ----------------------------------------------------------------
bm_pls_fit <- pls(
  X = biom_tr[, -6], biom_tr[, 6], ncomp = 3, scale = TRUE
)

bm_pls_tr <- bm_pls_fit$variates$X %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "comp", replacement = "PLS")

bm_pls_te <- predict(bm_pls_fit, biom_te[, -6])$variates %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "dim", replacement = "PLS")

# pls 2 outcomes ---------------------------------------------------------------
bm_pls_multi_fit <- pls(
  X = biom_tr[, -c(1, 6)], biom_tr[, c(1, 6)], ncomp = 3, scale = TRUE
)

bm_pls_multi_tr <- bm_pls_multi_fit$variates$X %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "comp", replacement = "PLS")

bm_pls_multi_te <- predict(bm_pls_multi_fit, biom_te[, -c(1, 6)])$variates %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "dim", replacement = "PLS")

# spls 1 outcome ---------------------------------------------------------------
bm_spls_fit <- spls(
  X = biom_tr[, -6], biom_tr[, 6], ncomp = 3, keepX = c(3, 3, 3), scale = TRUE
)

bm_spls_tr <- bm_spls_fit$variates$X %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "comp", replacement = "PLS")

bm_spls_te <- predict(bm_spls_fit, biom_te[, -6])$variates %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "dim", replacement = "PLS")

# spls 2 outcomes --------------------------------------------------------------
bm_spls_multi_fit <- spls(
  X = biom_tr[, -c(1, 6)], biom_tr[, c(1, 6)], ncomp = 3, keepX = c(3, 3, 3), scale = TRUE
)

bm_spls_multi_tr <- bm_spls_multi_fit$variates$X %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "comp", replacement = "PLS")

bm_spls_multi_te <- predict(bm_spls_multi_fit, biom_te[, -c(1, 6)])$variates %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "dim", replacement = "PLS")

# plsda 1 outcome --------------------------------------------------------------
cell_plsda_fit <- plsda(
  X = cell_tr[, -1], cell_tr[[1]], ncomp = 3, scale = TRUE
)

cell_plsda_tr <- cell_plsda_fit$variates$X %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "comp", replacement = "PLS")

cell_plsda_te <- predict(cell_plsda_fit, cell_te)$variates %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "dim", replacement = "PLS")

# splsda 1 outcome -------------------------------------------------------------
cell_splsda_fit <- splsda(
  X = cell_tr[, -1], cell_tr[[1]], ncomp = 3, keepX = c(50, 50, 50), scale = TRUE
)

cell_splsda_tr <- cell_splsda_fit$variates$X %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "comp", replacement = "PLS")

cell_splsda_te <- predict(cell_splsda_fit, cell_te)$variates %>%
  as_tibble() %>%
  rename_with(str_replace, pattern = "dim", replacement = "PLS")

# Saving -----------------------------------------------------------------------
save(
  bm_pls_tr,
  bm_pls_te,
  bm_pls_multi_tr,
  bm_pls_multi_te,
  bm_spls_tr,
  bm_spls_te,
  bm_spls_multi_tr,
  bm_spls_multi_te,
  cell_plsda_tr,
  cell_plsda_te,
  cell_splsda_tr,
  cell_splsda_te,
  file = testthat::test_path("test_pls_new.RData"),
  version = 2
)
