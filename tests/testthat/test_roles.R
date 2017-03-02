library(testthat)
library(magrittr)
library(recipes)
library(tibble)

data(biomass)

test_that('default method', {
  rec <- recipe(x = biomass)
  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = NA,
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('changing roles with character string', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, "sample", role = "some other role")
  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("some other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change existing role with character string', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, "sample", role = "some other role")
  rec <- add_role(rec, "sample", role = "other other role")
  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("other other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})


test_that('changing roles with formula', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, ~ sample, role = "some other role")
  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("some other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('changing roles with bad formula', {
  rec <- recipe(x = biomass)
  expect_error(add_role(rec, ~ ., role = "some other role"))
})

