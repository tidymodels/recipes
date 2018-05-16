library(testthat)
library(recipes)
library(tibble)
library(tidyselect)
library(rlang)

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
    terms_select(info = info1, quos(all_predictors())),
    info1$variable
  )
  expect_error(terms_select(info = info1, quos(all_outcomes())))
  expect_equal(
    terms_select(info = info2, quos(all_outcomes())),
    "HHV"
  )
  expect_equal(
    terms_select(info = info2, quos(has_role("splitting indicator"))),
    "dataset"
  )
})

test_that('simple type selections', {
  expect_equal(
    terms_select(info = info1, quos(all_numeric())),
    c("age", "height")
  )
  expect_equal(
    terms_select(info = info1, quos(has_type("date"))),
    "date"
  )
  expect_equal(
    terms_select(info = info1, quos(all_nominal())),
    c("diet", "location", "Class")
  )
})


test_that('simple name selections', {
  expect_equal(
    terms_select(info = info1, quos(matches("e$"))),
    c("age", "date")
  )
  expect_equal(
    terms_select(info = info2, quos(contains("gen"))),
    c("hydrogen", "oxygen", "nitrogen")
  )
  expect_equal(
    terms_select(info = info2, quos(contains("gen"), -nitrogen)),
    c("hydrogen", "oxygen")
  )
  expect_equal(
    terms_select(info = info1, quos(date, age)),
    c("date", "age")
  )

  expect_equal(
    terms_select(info = info1, quos(-age, date)),
    c("diet", "height", "location", "date", "Class")
  )
  expect_equal(
    terms_select(info = info1, quos(date, -age)),
    "date"
  )
  expect_error(terms_select(info = info1, quos(log(date))))
  expect_error(terms_select(info = info1, quos(date:age)))
  expect_error(terms_select(info = info1, quos(I(date:age))))
  expect_error(terms_select(info = info1, quos(matches("blahblahblah"))))
  expect_error(terms_select(info = info1))
})


test_that('combinations', {
  expect_equal(
    terms_select(info = info2, quos(matches("[hH]"), -all_outcomes())),
    "hydrogen"
  )
  expect_equal(
    terms_select(info = info2, quos(all_numeric(), -all_predictors())),
    "HHV"
  )
  expect_equal(
    terms_select(info = info2, quos(all_numeric(), -all_predictors(), dataset)),
    c("HHV", "dataset")
  )
  expect_equal(
    terms_select(info = info2, quos(all_numeric(), -all_predictors(), dataset, -dataset)),
    "HHV"
  )
})



