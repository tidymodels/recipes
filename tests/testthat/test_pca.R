library(testthat)
library(recipes)
data(biomass)

context("PCA")


biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

# Note: some tests convert to data frame prior to testing
# https://github.com/tidyverse/dplyr/issues/2751

test_that('correct PCA values', {
  pca_extract <- rec %>%
    step_center(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_scale(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur,
             options = list(retx = TRUE), id = "")

  pca_extract_trained <- prep(pca_extract, training = biomass_tr, verbose = FALSE)

  pca_pred <- bake(pca_extract_trained, new_data = biomass_te, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- prcomp(biomass_tr[, 3:7], center = TRUE, scale. = TRUE, retx = TRUE)
  pca_pred_exp <- predict(pca_exp, biomass_te[, 3:7])[, 1:pca_extract$steps[[3]]$num_comp]

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)

  tidy_exp_un <- tibble(
    terms = c("carbon", "hydrogen", "oxygen" ,"nitrogen", "sulfur"),
    value = rep(NA_real_, 5),
    component = rep(NA_character_, 5),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(pca_extract, number = 3))

  pca_obj <- prcomp(
    x = biomass_tr[, c("carbon", "hydrogen", "oxygen" ,"nitrogen", "sulfur")],
    scale. = TRUE)$rotation
  pca_obj <- as.data.frame(pca_obj)
  pca_obj <- utils::stack(pca_obj)

  tidy_exp_tr <- tibble(
    terms = rep(tidy_exp_un$terms, pca_extract_trained$steps[[3]]$num_comp),
    value = pca_obj$values,
    component = as.character(pca_obj$ind),
    id = ""
  )
  expect_equal(
    as.data.frame(tidy_exp_tr),
    as.data.frame(tidy(pca_extract_trained, number = 3))
  )
})

test_that('correct PCA values with threshold', {
  pca_extract <- rec %>%
    step_center(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_scale(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, threshold = .5)

  pca_extract_trained <- prep(pca_extract, training = biomass_tr, verbose = FALSE)
  pca_exp <- prcomp(biomass_tr[, 3:7], center = TRUE, scale. = TRUE, retx = TRUE)
  # cumsum(pca_exp$sdev^2)/sum(pca_exp$sdev^2)

  expect_equal(pca_extract_trained$steps[[3]]$num_comp, 2)
})


test_that('Reduced rotation size', {
  pca_extract <- rec %>%
    step_center(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_scale(carbon, hydrogen, oxygen ,nitrogen, sulfur) %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, num_comp = 3)

  pca_extract_trained <- prep(pca_extract, training = biomass_tr, verbose = FALSE)

  pca_pred <- bake(pca_extract_trained, new_data = biomass_te, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- prcomp(biomass_tr[, 3:7], center = TRUE, scale. = TRUE, retx = TRUE)
  pca_pred_exp <- predict(pca_exp, biomass_te[, 3:7])[, 1:3]
  rownames(pca_pred_exp) <- NULL

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)
})

test_that('deprecated arg', {
  expect_message(
    rec %>%
      step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, num = 2)
  )
})

test_that('printing', {
  pca_extract <- rec %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur)
  expect_output(print(pca_extract))
  expect_output(prep(pca_extract, training = biomass_tr, verbose = TRUE))
})

