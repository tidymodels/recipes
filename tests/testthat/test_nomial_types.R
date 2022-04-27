library(recipes)
library(dplyr)
library(testthat)

# ----------------------------------------------------------------

library(modeldata)
data("Sacramento")

Sacramento_chr <-
  Sacramento %>%
  mutate(type = as.character(type))

Sacramento_fac <-
  Sacramento %>%
  mutate(city = as.factor(city))

Sacramento_all_fac <-
  Sacramento_fac %>%
  mutate(zip = as.factor(zip))

# ----------------------------------------------------------------

test_that("factors all the way down", {
  tr <-
    Sacramento_all_fac %>%
    slice(1:500)

  te <-
    Sacramento_all_fac %>%
    slice(501:1000)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


test_that("factors all the way down with skipping", {
  tr <-
    Sacramento_all_fac %>%
    slice(1:500)

  te <-
    Sacramento_all_fac %>%
    slice(501:1000) %>%
    select(-Class)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

# ----------------------------------------------------------------

test_that("mixed nominal data", {
  tr <-
    Sacramento_fac %>%
    slice(1:500)
  te <-
    Sacramento_fac %>%
    slice(501:1000)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


test_that("mixed nominal data with skipping", {
  tr <-
    Sacramento_fac %>%
    slice(1:500)
  te <-
    Sacramento_fac %>%
    slice(501:1000) %>%
    select(-Class)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


# ----------------------------------------------------------------

test_that("no factors", {
  tr <-
    Sacramento_chr %>%
    slice(1:500)
  te <-
    Sacramento_chr %>%
    slice(501:1000)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


test_that("no factors with skipping", {
  tr <-
    Sacramento_chr %>%
    slice(1:500)
  te <-
    Sacramento_chr %>%
    slice(501:1000) %>%
    select(-Class)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


# ----------------------------------------------------------------

test_that("missing factors", {
  tr <-
    Sacramento_fac %>%
    slice(1:500)
  te <-
    Sacramento_chr %>%
    slice(501:1000)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_snapshot(check_nominal_type(te, rec$orig_lvls))
})

test_that("missing factors with skipping", {
  tr <-
    Sacramento_fac %>%
    slice(1:500)
  te <-
    Sacramento_chr %>%
    slice(501:1000) %>%
    select(-Class)

  rec <-
    recipe(type ~ ., data = tr) %>%
    prep(training = tr)

  expect_snapshot(check_nominal_type(te, rec$orig_lvls))
})
