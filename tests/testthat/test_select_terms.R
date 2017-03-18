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
    select_terms(info = info1, tidy_quotes(all_predictors())),
    info1$variable
  )
  expect_error(select_terms(info = info1, tidy_quotes(all_outcomes())))
  expect_equal(
    select_terms(info = info2, tidy_quotes(all_outcomes())),
    "HHV"
  )
  expect_equal(
    select_terms(info = info2, tidy_quotes(has_role("splitting indicator"))),
    "dataset"
  )
})

test_that('simple type selections', {
  expect_equal(
    select_terms(info = info1, tidy_quotes(all_numeric())),
    c("age", "height")
  )
  expect_equal(
    select_terms(info = info1, tidy_quotes(has_type("date"))),
    "date"
  )
  expect_equal(
    select_terms(info = info1, tidy_quotes(all_nominal())),
    c("diet", "location")
  )
})


test_that('simple name selections', {
  expect_equal(
    select_terms(info = info1, tidy_quotes(matches("e$"))),
    c("age", "date")
  )
  expect_equal(
    select_terms(info = info2, tidy_quotes(contains("gen"))),
    c("hydrogen", "oxygen", "nitrogen")
  )
  expect_equal(
    select_terms(info = info2, tidy_quotes(contains("gen"), -nitrogen)),
    c("hydrogen", "oxygen")
  )
  expect_equal(
    select_terms(info = info1, tidy_quotes(date, age)),
    c("date", "age")
  )
  ## This is weird but consistent with `dplyr::select_vars`
  expect_equal(
    select_terms(info = info1, tidy_quotes(-age, date)),
    c("diet", "location", "height", "date")
  )
  expect_equal(
    select_terms(info = info1, tidy_quotes(date, -age)),
    "date"
  )
  expect_error(select_terms(info = info1, tidy_quotes(log(date))))
  expect_error(select_terms(info = info1, tidy_quotes(date:age)))
  expect_error(select_terms(info = info1, tidy_quotes(I(date:age))))
  expect_error(select_terms(info = info1, tidy_quotes(matches("blahblahblah"))))
  expect_error(select_terms(info = info1))
})


test_that('combinations', {
  expect_equal(
    select_terms(info = info2, tidy_quotes(matches("[hH]"), -all_outcomes())),
    "hydrogen"
  )
  expect_equal(
    select_terms(info = info2, tidy_quotes(all_numeric(), -all_predictors())),
    "HHV"
  )
  expect_equal(
    select_terms(info = info2, tidy_quotes(all_numeric(), -all_predictors(), dataset)),
    c("HHV", "dataset")
  )
  expect_equal(
    select_terms(info = info2, tidy_quotes(all_numeric(), -all_predictors(), dataset, -dataset)),
    "HHV"
  )
})



