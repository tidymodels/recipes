library(testthat)
library(rlang)
library(recipes)

library(modeldata)
data(biomass)

context("Testing center and scale")

means <- vapply(biomass[, 3:7], mean, c(mean = 0))
sds <- vapply(biomass[, 3:7], sd, c(sd = 0))

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

# Note: some tests convert to data frame prior to testing
# https://github.com/tidyverse/dplyr/issues/2751

test_that('correct means and std devs', {
  standardized <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "center") %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "scale")

  cent_tibble_un <-
    tibble(terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
           value = rep(na_dbl, 5),
           id = standardized$steps[[1]]$id)
  scal_tibble_un <- cent_tibble_un
  scal_tibble_un$id <- standardized$steps[[2]]$id

  expect_equal(tidy(standardized, 1), cent_tibble_un)
  expect_equal(as.data.frame(tidy(standardized, 2)), as.data.frame(scal_tibble_un))

  standardized_trained <- prep(standardized, training = biomass)

  cent_tibble_tr <-
    tibble(terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
           value = means,
           id = standardized$steps[[1]]$id)
  scal_tibble_tr <-
    tibble(terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
           value = sds,
           id = standardized$steps[[2]]$id)

  expect_equal(tidy(standardized_trained, 1), cent_tibble_tr)
  expect_equal(
    as.data.frame(tidy(standardized_trained, 2)),
    as.data.frame(scal_tibble_tr)
  )

  expect_equal(standardized_trained$steps[[1]]$means, means)
  expect_equal(standardized_trained$steps[[2]]$sds, sds)
})

test_that('scale by factor of 1 or 2', {
  standardized <- rec %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "scale", factor = 2)

  standardized_trained <- prep(standardized, training = biomass)

  scal_tibble_tr <-
    tibble(terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
           value = sds*2,
           id = standardized$steps[[1]]$id)

  expect_equal(tidy(standardized_trained, 1), scal_tibble_tr)

  expect_equal(standardized_trained$steps[[1]]$sds, 2*sds)

  expect_warning(
    not_recommended_standardized_input <- rec %>%
      step_scale(carbon, id = "scale", factor = 3) %>%
      prep(training = biomass)
  )

})

test_that('training in stages', {
  at_once <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "center") %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "scale")

  at_once_trained <- prep(at_once, training = biomass)

  ## not train in stages
  center_first <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "center")
  center_first_trained <- prep(center_first, training = biomass)
  in_stages <- center_first_trained %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "scale")
  in_stages_trained <- prep(in_stages)
  in_stages_retrained <-
    prep(in_stages, training = biomass, fresh = TRUE)

  expect_equal(at_once_trained, in_stages_trained)
  expect_equal(at_once_trained, in_stages_retrained)
})


test_that('single predictor', {
  standardized <- rec %>%
    step_center(carbon) %>%
    step_scale(hydrogen)

  standardized_trained <- prep(standardized, training = biomass)
  results <- bake(standardized_trained, biomass)

  exp_res <- biomass[, 3:8]
  exp_res$carbon <- exp_res$carbon - mean(exp_res$carbon)
  exp_res$hydrogen <- exp_res$hydrogen / sd(exp_res$hydrogen)

  expect_equal(as.data.frame(results), exp_res[, colnames(results)])
})


test_that('printing', {
  standardized <- rec %>%
    step_center(carbon) %>%
    step_scale(hydrogen)
  expect_output(print(standardized))
  expect_output(prep(standardized, training = biomass, verbose = TRUE))
})

test_that('correct means and std devs for step_norm', {
  standardized <- rec %>%
    step_normalize(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "norm")

  vrs <- c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur")
  norm_tibble_un <-
    tibble(terms = vrs,
           value = rep(na_dbl, 5),
           statistic = rep(na_chr, 5),
           id = standardized$steps[[1]]$id)

  expect_equal(tidy(standardized, 1), norm_tibble_un)

  standardized_trained <- prep(standardized, training = biomass)

  norm_tibble_tr <-
    tibble(terms = c(vrs, vrs),
           value = c(means, sds),
           statistic = rep(c("mean", "sd"), each = 5),
           id = standardized$steps[[1]]$id)

  expect_equal(tidy(standardized_trained, 1), norm_tibble_tr)
})
