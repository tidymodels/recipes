library(testthat)
library(recipes)
library(modeldata)
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
    scale. = TRUE)
  variances <- pca_obj$sdev^2
  pca_obj <- pca_obj$rotation
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

  var_obj <- tidy(pca_extract_trained, number = 3, type = "variance")
  expect_equal(
    var_obj$value[var_obj$terms == "variance"],
    variances
  )
  expect_equal(
    var_obj$value[var_obj$terms == "cumulative variance"],
    cumsum(variances)
  )
  expect_equal(
    var_obj$value[var_obj$terms == "percent variance"],
    variances / sum(variances) * 100
  )
  expect_equal(
    var_obj$value[var_obj$terms == "cumulative percent variance"],
    cumsum(variances) / sum(variances) * 100
  )
  expect_error(tidy(pca_extract_trained, number = 3, type = "variances"),
               "variance")

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

test_that('printing', {
  pca_extract <- rec %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur)
  expect_output(print(pca_extract))
  expect_output(prep(pca_extract, training = biomass_tr, verbose = TRUE))
})


test_that('No PCA comps', {
  pca_extract <- rec %>%
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, num_comp = 0)

  pca_extract_trained <- prep(pca_extract, training = biomass_tr)
  expect_equal(
    names(juice(pca_extract_trained)),
    names(biomass_tr)[-(1:2)]
  )
  expect_true(all(is.na(pca_extract_trained$steps[[1]]$res$rotation)))
  expect_output(print(pca_extract_trained),
                regexp = "No PCA components were extracted")
  expect_true(all(is.na(tidy(pca_extract_trained, 1)$value)))
})

test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_pca(all_predictors())
  rec_param <- tunable.step_pca(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})
