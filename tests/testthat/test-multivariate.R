library(tibble)
library(recipes)
skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

test_that("multivariate outcome", {
  raw_recipe <- recipe(
    carbon + hydrogen ~ oxygen + nitrogen + sulfur,
    data = biomass
  )
  rec <- raw_recipe |>
    step_center(all_outcomes()) |>
    step_scale(all_predictors())

  rec_trained <- prep(rec, training = biomass)

  results <- bake(rec_trained, head(biomass))

  exp_res <- biomass

  pred <- c("oxygen", "nitrogen", "sulfur")
  outcome <- c("carbon", "hydrogen")
  for (i in pred) {
    exp_res[, i] <- exp_res[, i] / sd(exp_res[, i])
  }
  for (i in outcome) {
    exp_res[, i] <- exp_res[, i] - mean(exp_res[, i])
  }

  expect_equal(rec$term_info$variable[rec$term_info$role == "outcome"], outcome)
  expect_equal(rec$term_info$variable[rec$term_info$role == "predictor"], pred)
  expect_equal(exp_res[1:6, colnames(results)], as.data.frame(results))
})
