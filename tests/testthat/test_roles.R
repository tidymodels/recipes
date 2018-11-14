library(testthat)
library(recipes)
library(tibble)

context("Changing roles")


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
  rec <- update_role(rec, sample, new_role = "some other role")
  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("some other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change existing role', {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "some other role")
  rec <- update_role(rec, sample, new_role = "other other role")

  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("other other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change multiple existing role for same variable', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role")
  # changes both at once
  rec <- update_role(rec, sample, new_role = "other other role")

  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = rep(c("nominal", "numeric"), c(3, 6)),
                    role = rep(c("other other role", NA), c(2, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change only 1 role of variable with multiple roles', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role")
  # changes only 1
  rec <- update_role(rec, sample, new_role = "other other role", old_role = "some other role")

  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = rep(c("nominal", "numeric"), c(3, 6)),
                    role = c(NA, "other other role", rep(NA, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change every role of 2 variables', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, dataset, new_role = "some other role")
  # changes only 1
  rec <- update_role(rec, sample, dataset, new_role = "other other role")

  exp_res <- tibble(variable = c(
                      rep(c("sample", "dataset"), c(2,2)),
                      setdiff(colnames(biomass), c("sample", "dataset"))
                    ),
                    type = rep(c("nominal", "numeric"), c(4, 6)),
                    role = c(rep("other other role", 4), rep(NA, 6)),
                    source = "original")

  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('update only NA role', {
  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, dataset, new_role = "some other role")
  # changes only 1
  rec <- update_role(rec, sample, dataset, new_role = "other other role", old_role = NA)

  exp_res <- tibble(variable = c(
                      rep(c("sample", "dataset"), c(2,2)),
                      setdiff(colnames(biomass), c("sample", "dataset"))
                    ),
                    type = rep(c("nominal", "numeric"), c(4, 6)),
                    role = c(rep(c("other other role", "some other role"), 2), rep(NA, 6)),
                    source = "original")

  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('new role', {

  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role")

  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = rep(c("nominal", "numeric"), c(3, 6)),
                    role = rep(c(NA, "some other role", NA), c(1, 1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('new role with specified type', {

  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role", new_type = "new type")

  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = c("nominal", "new type", "nominal", rep("numeric", 6)),
                    role = rep(c(NA, "some other role", NA), c(1, 1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('add new role when two already exist with different types', {

  # type of the first existing role found is used
  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role", new_type = "new type")
  rec <- add_role(rec, sample, new_role = "another role")

  exp_res <- tibble(variable = c("sample", "sample", colnames(biomass)),
                    type = c("nominal", "new type", "nominal", "nominal", rep("numeric", 6)),
                    role = c(NA, "some other role", "another role", rep(NA, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('existing role is skipped', {

  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role")
  # skip me
  expect_warning(
    rec <- add_role(rec, sample, new_role = "some other role")
  )

  # also tests the order, new roles come directly after old ones
  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = rep(c("nominal", "numeric"), c(3, 6)),
                    role = rep(c(NA, "some other role", NA), c(1, 1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)

})

test_that('existing role is skipped, but new one is added', {

  rec <- recipe(x = biomass)
  rec <- add_role(rec, sample, new_role = "some other role")
  # partially skip me
  expect_warning(
    rec <- add_role(rec, sample, dataset, new_role = "some other role")
  )

  exp_res <- tibble(variable = c(
                      rep(c("sample", "dataset"), c(2,2)),
                      setdiff(colnames(biomass), c("sample", "dataset"))
                    ),
                    type = rep(c("nominal", "numeric"), c(4, 6)),
                    role = c(NA, "some other role", NA, "some other role", rep(NA, 6)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('NA role can be skipped', {

  rec <- recipe(x = biomass)
  # skip me
  expect_warning(
    rec <- add_role(rec, sample, new_role = NA)
  )

  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = NA,
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)

})

test_that('NA role can be added', {

  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "role1")
  rec <- add_role(rec, sample, new_role = NA)

  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = rep(c("nominal", "numeric"), c(3, 6)),
                    role = c("role1", rep(NA, 8)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)

})

test_that('New type for an existing role can be added', {

  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "role1")
  rec <- add_role(rec, sample, new_role = "role1", new_type = "text")

  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = c(c("nominal", "text", "nominal"), rep("numeric", 6)),
                    role = c("role1", "role1", rep(NA, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)

})

test_that('bad args', {
  expect_error(
    recipe(x = biomass) %>%
      add_role(carbon, new_role = letters[1:2]),
    "`new_role` must have length 1."
  )

  expect_error(
    expect_warning(
      recipe(x = biomass) %>%
        add_role(new_role = "some other role")
    ),
    "At least one selector"
  )

  expect_error(
    recipe(x = biomass) %>%
      add_role(carbon, new_role = "a", new_type = letters[1:2]),
    "`new_type` must have length 1."
  )

  expect_error(
    recipe(x = biomass) %>%
      update_role(carbon, new_role = c("a", "b")),
    "`new_role` must have length 1."
  )

  expect_error(
    recipe(x = biomass) %>%
      update_role(carbon, old_role = c("a", "b")),
    "`old_role` must have length 1."
  )

})


# ------------------------------------------------------------------------------
# Multiples roles + Selection testing

test_that("adding multiple roles/types does not duplicate prepped columns", {

  rec <- recipe(HHV ~ ., data = biomass)

  # second role
  expect_equal(
    rec %>%
      add_role(carbon, new_role = "carb") %>%
      prep(training = biomass, retain = TRUE) %>%
      juice() %>%
      ncol(),

    8
  )

  # second type
  expect_equal(
    rec %>%
      add_role(carbon, new_type = "carb") %>%
      prep(training = biomass, retain = TRUE) %>%
      juice() %>%
      ncol(),

    8
  )

})

test_that("type selectors can be combined", {

  rec <- recipe(HHV ~ ., data = biomass)

  prepped <- rec %>%
    add_role(carbon, new_role = "predictor", new_type = "carb") %>%
    step_center(all_numeric(), -has_type("carb")) %>%
    prep(training = biomass, retain = TRUE)

  expect_equal(
    names(prepped$steps[[1]]$means),
    c("hydrogen", "oxygen", "nitrogen", "sulfur", "HHV")
  )

})

test_that("step_rm() removes ALL mention of variables with that role", {

  rec <- recipe(HHV ~ ., data = biomass)

  rec_prepped <- rec %>%
    add_role(carbon, new_role = "predictor", new_type = "carb") %>%
    step_rm(has_type("carb")) %>%
    prep(training = biomass, retain = TRUE) %>%
    summary()

  expect_false("carbon" %in% rec_prepped$variable)
})
