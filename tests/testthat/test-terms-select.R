library(testthat)
library(recipes)
library(tibble)
library(tidyselect)
library(rlang)

skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")
rec1 <- recipe(~., data = Sacramento)
info1 <- summary(rec1)

data(biomass, package = "modeldata")
rec2 <- recipe(biomass) %>%
  update_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
    new_role = "predictor"
  ) %>%
  update_role(HHV, new_role = "outcome") %>%
  update_role(sample, new_role = "id variable") %>%
  update_role(dataset, new_role = "splitting indicator")
info2 <- summary(rec2)

test_that("terms_select() is deprecated", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_snapshot(terms_select(info = info1, quos(all_predictors())))
})

test_that("simple role selections", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    terms_select(info = info1, quos(all_predictors())),
    info1$variable
  )
  expect_snapshot(error = TRUE,
    terms_select(info = info1, quos(all_outcomes()))
  )
  expect_equal(
    terms_select(info = info2, quos(all_outcomes())),
    "HHV"
  )
  expect_equal(
    terms_select(info = info2, quos(has_role("splitting indicator"))),
    "dataset"
  )
})

test_that("simple type selections", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    terms_select(info = info1, quos(all_numeric())),
    c("beds", "baths", "sqft", "price", "latitude", "longitude")
  )
  expect_equal(
    terms_select(info = info1, quos(has_type("nominal"))),
    c("city", "zip", "type")
  )
  expect_equal(
    terms_select(info = info1, quos(all_nominal())),
    c("city", "zip", "type")
  )
})


test_that("simple name selections", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    terms_select(info = info1, quos(matches("s$"))),
    c("beds", "baths")
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
    terms_select(info = info1, quos(beds, sqft)),
    c("beds", "sqft")
  )

  expect_equal(
    terms_select(info = info1, quos(-sqft, beds)),
    c("city", "zip", "beds", "baths", "type", "price", "latitude", "longitude")
  )
  expect_equal(
    terms_select(info = info1, quos(beds, -sqft)),
    "beds"
  )
  expect_snapshot(error = TRUE,
    terms_select(info = info1, quos(log(beds)))
  )
  expect_snapshot(error = TRUE,
    terms_select(info = info1, quos(beds:sqft))
  )
  expect_snapshot(error = TRUE,
    terms_select(info = info1, quos(I(beds:sqft)))
  )
  expect_snapshot(error = TRUE,
    terms_select(info = info1, quos(matches("blahblahblah")))
  )
  expect_snapshot(error = TRUE,
    terms_select(info = info1)
  )
})


test_that("combinations", {
  rlang::local_options(lifecycle_verbosity = "quiet")

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

test_that("namespaced selectors", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    terms_select(info = info1, quos(tidyselect::matches("e$"))),
    terms_select(info = info1, quos(matches("e$")))
  )
  expect_equal(
    terms_select(info = info1, quos(dplyr::matches("e$"))),
    terms_select(info = info1, quos(matches("e$")))
  )
  expect_equal(
    terms_select(info = info1, quos(recipes::all_predictors())),
    terms_select(info = info1, quos(all_predictors()))
  )
})

test_that("new dplyr selectors", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  vnames <- c("hydrogen", "carbon")
  expect_error(
    rec_1 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(all_of(c("hydrogen", "carbon"))) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_1$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_error(
    rec_2 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(all_of(!!vnames)) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_2$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_error(
    rec_3 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(any_of(c("hydrogen", "carbon"))) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_3$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_error(
    rec_4 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(any_of(c("hydrogen", "carbon", "bourbon"))) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_4$steps[[1]]$means), c("hydrogen", "carbon"))
})
