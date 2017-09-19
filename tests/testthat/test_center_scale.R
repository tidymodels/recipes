library(testthat)
context("Testing center and scale")

library(recipes)

means <- vapply(biomass[, 3:7], mean, c(mean = 0))
sds <- vapply(biomass[, 3:7], sd, c(sd = 0))

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('correct means and std devs', {
  standardized <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur)

  cent_tibble_un <-
    tibble(terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
           value = rep(na_dbl, 5))

  expect_equal(tidy(standardized, 1), cent_tibble_un)
  expect_equal(tidy(standardized, 2), cent_tibble_un)

  standardized_trained <- prep(standardized, training = biomass, verbose = FALSE)

  cent_tibble_tr <-
    tibble(terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
           value = means)
  scal_tibble_tr <-
    tibble(terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
           value = sds)

  expect_equal(tidy(standardized_trained, 1), cent_tibble_tr)
  expect_equal(tidy(standardized_trained, 2), scal_tibble_tr)

  expect_equal(standardized_trained$steps[[1]]$means, means)
  expect_equal(standardized_trained$steps[[2]]$sds, sds)
})

test_that('training in stages', {
  at_once <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur)

  at_once_trained <- prep(at_once, training = biomass, verbose = FALSE)

  ## not train in stages
  center_first <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur)
  center_first_trained <- prep(center_first, training = biomass, verbose = FALSE)
  in_stages <- center_first_trained %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur)
  in_stages_trained <- prep(in_stages, training = biomass, verbose = FALSE)
  in_stages_retrained <- prep(in_stages, training = biomass, verbose = FALSE, fresh = TRUE)

  expect_equal(at_once_trained, in_stages_trained)
  expect_equal(at_once_trained, in_stages_retrained)

})


test_that('single predictor', {
  standardized <- rec %>%
    step_center(carbon) %>%
    step_scale(hydrogen)

  standardized_trained <- prep(standardized, training = biomass, verbose = FALSE)
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

