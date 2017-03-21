library(testthat)
library(recipes)
library(tibble)
library(dplyr)

data(okc)
rec1 <- recipe(~ ., data = okc)
info1 <- summary(rec1)

data(biomass)
rec2 <- recipe(biomass) %>%
  add_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
           new_role = "predictor") %>%
  add_role(HHV, new_role = "outcome") %>%
  add_role(sample, new_role = "id variable") %>%
  add_role(dataset, new_role = "splitting indicator")
info2 <- summary(rec2)

test_that('simple role selections', {
  expect_equal(
    select_terms(info = info1, dots_quosures(all_predictors())),
    info1$variable
  )
  expect_error(select_terms(info = info1, dots_quosures(all_outcomes())))
  expect_equal(
    select_terms(info = info2, dots_quosures(all_outcomes())),
    "HHV"
  )
  expect_equal(
    select_terms(info = info2, dots_quosures(has_role("splitting indicator"))),
    "dataset"
  )
})

test_that('simple type selections', {
  expect_equal(
    select_terms(info = info1, dots_quosures(all_numeric())),
    c("age", "height")
  )
  expect_equal(
    select_terms(info = info1, dots_quosures(has_type("date"))),
    "date"
  )
  expect_equal(
    select_terms(info = info1, dots_quosures(all_nominal())),
    c("diet", "location")
  )
})


test_that('simple name selections', {
  expect_equal(
    select_terms(info = info1, dots_quosures(matches("e$"))),
    c("age", "date")
  )
  expect_equal(
    select_terms(info = info2, dots_quosures(contains("gen"))),
    c("hydrogen", "oxygen", "nitrogen")
  )
  expect_equal(
    select_terms(info = info2, dots_quosures(contains("gen"), -nitrogen)),
    c("hydrogen", "oxygen")
  )
  expect_equal(
    select_terms(info = info1, dots_quosures(date, age)),
    c("date", "age")
  )
  ## This is weird but consistent with `dplyr::select_vars`
  expect_equal(
    select_terms(info = info1, dots_quosures(-age, date)),
    c("diet", "location", "height", "date")
  )
  expect_equal(
    select_terms(info = info1, dots_quosures(date, -age)),
    "date"
  )
  expect_error(select_terms(info = info1, dots_quosures(log(date))))
  expect_error(select_terms(info = info1, dots_quosures(date:age)))
  expect_error(select_terms(info = info1, dots_quosures(I(date:age))))
  expect_error(select_terms(info = info1, dots_quosures(matches("blahblahblah"))))
  expect_error(select_terms(info = info1))
})


test_that('combinations', {
  expect_equal(
    select_terms(info = info2, dots_quosures(matches("[hH]"), -all_outcomes())),
    "hydrogen"
  )
  expect_equal(
    select_terms(info = info2, dots_quosures(all_numeric(), -all_predictors())),
    "HHV"
  )
  expect_equal(
    select_terms(info = info2, dots_quosures(all_numeric(), -all_predictors(), dataset)),
    c("HHV", "dataset")
  )
  expect_equal(
    select_terms(info = info2, dots_quosures(all_numeric(), -all_predictors(), dataset, -dataset)),
    "HHV"
  )
})



