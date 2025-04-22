library(testthat)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

test_that("basic steps", {
  rec_1 <-
    recipe(
      HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
      data = biomass_tr
    ) |>
    step_mutate(hydrogen = hydrogen / 2) |>
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) |>
    step_corr(all_predictors(), threshold = .6) |>
    prep()

  cols_1 <- c("hydrogen", "nitrogen", "sulfur", "HHV", "oxygen_o_carbon")

  expect_equal(
    names(bake(rec_1, new_data = NULL)),
    cols_1
  )
  expect_equal(
    names(bake(rec_1, biomass_te)),
    cols_1
  )

  expect_equal(
    names(bake(rec_1, new_data = NULL, all_predictors())),
    cols_1[cols_1 != "HHV"]
  )
  expect_equal(
    names(bake(rec_1, biomass_te, all_predictors())),
    cols_1[cols_1 != "HHV"]
  )
})

test_that("skipped steps", {
  rec_2 <-
    recipe(
      HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
      data = biomass_tr
    ) |>
    step_mutate(hydrogen = hydrogen / 2) |>
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) |>
    step_corr(all_predictors(), threshold = .6, skip = TRUE) |>
    prep()

  cols_1 <- c(
    "hydrogen",
    "nitrogen",
    "sulfur",
    "HHV",
    "oxygen_o_carbon"
  )

  # Ordering relative to the original specification of the recipe is
  # preserved (i.e. carbon was the first column specified, and since we
  # skipped the step that would drop it, it appears first in the result)
  cols_2 <- c(
    "carbon",
    "hydrogen",
    "oxygen",
    "nitrogen",
    "sulfur",
    "HHV",
    "hydrogen_o_carbon",
    "oxygen_o_carbon",
    "nitrogen_o_carbon"
  )

  expect_equal(
    names(bake(rec_2, new_data = NULL)),
    cols_1
  )
  expect_equal(
    names(bake(rec_2, biomass_te)),
    cols_2
  )

  expect_equal(
    names(bake(rec_2, new_data = NULL, all_predictors())),
    cols_1[cols_1 != "HHV"]
  )
  expect_equal(
    names(bake(rec_2, biomass_te, all_predictors())),
    cols_2[cols_2 != "HHV"]
  )
})

test_that("remove and add a column", {
  rec_3 <-
    recipe(
      HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
      data = biomass_tr
    ) |>
    step_rm(HHV) |>
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) |>
    step_corr(all_predictors(), threshold = .6) |>
    step_mutate(HHV = 17) |>
    prep()

  cols_3 <- c("hydrogen", "nitrogen", "sulfur", "oxygen_o_carbon", "HHV")

  expect_equal(
    names(bake(rec_3, new_data = NULL)),
    cols_3
  )
  expect_equal(
    names(bake(rec_3, biomass_te)),
    cols_3
  )

  expect_equal(
    names(bake(rec_3, new_data = NULL, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
  expect_equal(
    names(bake(rec_3, biomass_te, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
})

test_that("extra roles", {
  rec_4 <-
    recipe(
      HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
      data = biomass_tr
    ) |>
    add_role(nitrogen, new_role = "drummer") |>
    step_rm(HHV) |>
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) |>
    step_corr(all_predictors(), threshold = .6) |>
    step_mutate(HHV = 17) |>
    prep()

  cols_3 <- c("hydrogen", "nitrogen", "sulfur", "oxygen_o_carbon", "HHV")

  expect_equal(
    names(bake(rec_4, new_data = NULL)),
    cols_3
  )
  expect_equal(
    names(bake(rec_4, biomass_te)),
    cols_3
  )

  expect_equal(
    names(bake(rec_4, new_data = NULL, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
  expect_equal(
    names(bake(rec_4, biomass_te, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
})
