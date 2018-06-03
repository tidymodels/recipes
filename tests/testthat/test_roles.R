library(testthat)
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

test_that('changing roles', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role")
  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("some other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change existing role', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role")
  rec <- add_role(rec, sample, new_role = "other other role")
  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("other other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})


test_that('bad args', {
  expect_error(
    recipe(x = biomass) %>%
      add_role(carbon, new_role = letters[1:2])
  )
  expect_error(
    recipe(x = biomass) %>%
      add_role(new_role = "some other role")
  )
})
