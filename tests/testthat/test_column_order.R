context("Column ordering")

library(testthat)
library(tibble)
library(modeldata)
data(biomass)

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

test_that('basic steps', {
  rec_1 <-
    recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
           data = biomass_tr) %>%
    step_mutate(hydrogen = hydrogen/2) %>%
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) %>%
    step_corr(all_predictors(), threshold = .6) %>%
    prep()

  cols_1 <- c("hydrogen", "nitrogen", "sulfur", "HHV", "oxygen_o_carbon")

  expect_equal(
    names(juice(rec_1)),
    cols_1
  )
  expect_equal(
    names(bake(rec_1, biomass_te)),
    cols_1
  )

  expect_equal(
    names(juice(rec_1, all_predictors())),
    cols_1[cols_1 != "HHV"]
  )
  expect_equal(
    names(bake(rec_1, biomass_te, all_predictors())),
    cols_1[cols_1 != "HHV"]
  )
})


test_that('skipped steps', {
  rec_2 <-
    recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
           data = biomass_tr) %>%
    step_mutate(hydrogen = hydrogen/2) %>%
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) %>%
    step_corr(all_predictors(), threshold = .6, skip = TRUE) %>%
    prep()

  cols_1 <- c("hydrogen", "nitrogen", "sulfur", "HHV", "oxygen_o_carbon")

  cols_2 <- c("hydrogen", "nitrogen", "sulfur", "HHV", "oxygen_o_carbon",
              "carbon", "oxygen", "hydrogen_o_carbon", "nitrogen_o_carbon")

  expect_equal(
    names(juice(rec_2)),
    cols_1
  )
  expect_equal(
    names(bake(rec_2, biomass_te)),
    cols_2
  )

  expect_equal(
    names(juice(rec_2, all_predictors())),
    cols_1[cols_1 != "HHV"]
  )
  expect_equal(
    names(bake(rec_2, biomass_te, all_predictors())),
    cols_2[cols_2 != "HHV"]
  )
})


test_that('remove and add a column', {
  rec_3 <-
    recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
           data = biomass_tr) %>%
    step_rm(HHV) %>%
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) %>%
    step_corr(all_predictors(), threshold = .6) %>%
    step_mutate(HHV = 17) %>%
    prep()

  cols_3 <- c("hydrogen", "nitrogen", "sulfur", "oxygen_o_carbon", "HHV")

  expect_equal(
    names(juice(rec_3)),
    cols_3
  )
  expect_equal(
    names(bake(rec_3, biomass_te)),
    cols_3
  )

  expect_equal(
    names(juice(rec_3, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
  expect_equal(
    names(bake(rec_3, biomass_te, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
})

test_that('extra roles', {
  rec_4 <-
    recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
           data = biomass_tr) %>%
    add_role(nitrogen, new_role = "drummer") %>%
    step_rm(HHV) %>%
    step_ratio(hydrogen, oxygen, nitrogen, denom = vars(carbon)) %>%
    step_corr(all_predictors(), threshold = .6) %>%
    step_mutate(HHV = 17) %>%
    prep()

  cols_3 <- c("hydrogen", "nitrogen", "sulfur", "oxygen_o_carbon", "HHV")

  expect_equal(
    names(juice(rec_4)),
    cols_3
  )
  expect_equal(
    names(bake(rec_4, biomass_te)),
    cols_3
  )

  expect_equal(
    names(juice(rec_4, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
  expect_equal(
    names(bake(rec_4, biomass_te, all_predictors())),
    cols_3[cols_3 != "HHV"]
  )
})







