context("Changing roles")

library(testthat)
library(recipes)
library(tibble)

library(modeldata)
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

  expect_error(add_role(rec, sample, new_role = "some other role"))

  rec <- update_role(rec, sample, new_role = "some other role")
  rec <- update_role(rec, sample, new_role = "other other role")

  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("other other role", NA), c(1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change only 1 role of variable with multiple roles', {
  rec <- recipe(x = biomass)
  rec <-
    rec %>%
    update_role(sample, new_role = "role 1") %>%
    add_role(sample, new_role = "role 2")

  orig_roles <- rec

  # changes only 1
  rec <- update_role(rec, sample, new_role = "role 3", old_role = "role 1")

  exp_res <- summary(orig_roles)
  exp_res$role[exp_res$role == "role 1"] <- "role 3"
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('change every role of 2 variables', {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, dataset, new_role = "role 1")
  orig_roles <- summary(rec)
  rec <- update_role(rec, sample, dataset, new_role = "role 2")

  exp_res <- orig_roles
  exp_res$role[exp_res$role == "role 1"] <- "role 2"

  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('update only NA role', {
  rec <- recipe(x = biomass)
  orig_rec <- summary(rec)
  rec <- update_role(rec, sample, dataset, new_role = "some other role")

  exp_res <- orig_rec %>% arrange(variable)
  exp_res$role[exp_res$variable %in% c("sample", "dataset")] <- "some other role"

  expect_equal(summary(rec, TRUE) %>% arrange(variable), exp_res)
})

test_that('new role for existing NA role', {

  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "some other role")

  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = rep(c("some other role", NA), c(1, length(colnames(biomass)) - 1)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('new role with specified type', {

  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(rec, sample, new_role = "some other role", new_type = "new type")

  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = c("nominal", "new type", "nominal", rep("numeric", 6)),
                    role = rep(c("blah", "some other role", NA), c(1, 1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('add new role when two already exist with different types', {

  # type of the first existing role found is used
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(rec, sample, new_role = "some other role", new_type = "new type")
  rec <- add_role(rec, sample, new_role = "another role")

  exp_res <- tibble(variable = c("sample", "sample", colnames(biomass)),
                    type = c("nominal", "new type", "nominal", "nominal", rep("numeric", 6)),
                    role = c("blah", "some other role", "another role", rep(NA, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('existing role is skipped', {

  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(rec, sample, new_role = "some other role")

  # skip me
  expect_warning(
    rec <- add_role(rec, sample, new_role = "some other role"),
    "Role, 'some other role', already exists"
  )

  # also tests the order, new roles come directly after old ones
  exp_res <- tibble(variable = c("sample", colnames(biomass)),
                    type = rep(c("nominal", "numeric"), c(3, 6)),
                    role = rep(c("blah", "some other role", NA), c(1, 1, 7)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)

})

test_that('existing role is skipped, but new one is added', {

  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(rec, sample, new_role = "some other role")

  # partially skip me
  expect_warning(
    rec <- add_role(rec, sample, dataset, new_role = "some other role"),
    "Role, 'some other role', already exists"
  )

  exp_res <- tibble(variable = c(
                      rep(c("sample", "dataset"), c(2,2)),
                      setdiff(colnames(biomass), c("sample", "dataset"))
                    ),
                    type = rep(c("nominal", "numeric"), c(4, 6)),
                    role = c("blah", "some other role", NA, "some other role", rep(NA, 6)),
                    source = "original")
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that('cannot add roles if the current one is `NA`', {
  rec <- recipe(x = biomass)
  expect_error(add_role(rec, sample, sulfur), "No role currently exists")
})

test_that("`update_role()` cannot be ambiguous", {
  rec <- recipe(HHV ~ ., data = biomass)
  rec <- add_role(rec, sample, new_role = "x")

  expect_error(
    update_role(rec, sample, new_role = "y"),
    "`old_role` can only be `NULL` when"
  )
})

test_that("`new_role` cannot be `NA_character_`", {
  rec <- recipe(x = biomass)

  expect_error(
    add_role(rec, sample, new_role = NA_character_),
    "`new_role` must not be `NA`."
  )

  expect_error(
    update_role(rec, sample, new_role = NA_character_),
    "`new_role` must not be `NA`."
  )
})

test_that('remove roles', {

  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "role1")
  expect_error(
    rec <- remove_role(rec, sample, old_role = NA)
  )
  expect_error(
    rec <- remove_role(rec, sample)
  )

  expect_warning(
    remove_role(rec, sample, old_role = "non-existant"),
    "Column, 'sample', does not have role, 'non-existant'."
  )

  rec <- remove_role(rec, sample, old_role = "role1")

  exp_res <- tibble(variable = colnames(biomass),
                    type = rep(c("nominal", "numeric"), c(2, 6)),
                    role = NA_character_,
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

test_that("can use tidyselect ops in role selection", {
  rec <- recipe(mpg ~ ., mtcars) %>%
    step_center(all_predictors())

  # Swap "predictor" for "foo"
  rec <- update_role(
    rec,
    starts_with("c") & !carb,
    new_role = "foo",
    old_role = "predictor"
  )

  expect_identical(
    rec$term_info$role[rec$term_info$variable == "cyl"],
    "foo"
  )

  # Add "predictor" back
  rec <- add_role(
    rec,
    starts_with("c") & !carb,
    new_role = "predictor"
  )

  expect_identical(
    rec$term_info$role[rec$term_info$variable == "cyl"],
    c("foo", "predictor")
  )

  # Remove "foo"
  rec <- remove_role(
    rec,
    starts_with("c") & !carb,
    old_role = "foo"
  )

  expect_identical(
    rec$term_info$role[rec$term_info$variable == "cyl"],
    "predictor"
  )
})


test_that("empty dots and zero column selections return input with a warning", {
  rec <- recipe(x = biomass)

  expect_identical(
    expect_warning(
      add_role(rec),
      "No columns were selected in `add_role[(][)]`"
    ),
    rec
  )
  expect_identical(
    expect_warning(
      update_role(rec),
      "No columns were selected in `update_role[(][)]`"
    ),
    rec
  )
  expect_identical(
    expect_warning(
      remove_role(rec, old_role = "foo"),
      "No columns were selected in `remove_role[(][)]`"
    ),
    rec
  )

  expect_identical(
    expect_warning(
      add_role(rec, starts_with("foobar")),
      "No columns were selected in `add_role[(][)]`"
    ),
    rec
  )
  expect_identical(
    expect_warning(
      update_role(rec, starts_with("foobar")),
      "No columns were selected in `update_role[(][)]`"
    ),
    rec
  )
  expect_identical(
    expect_warning(
      remove_role(rec, starts_with("foobar"), old_role = "foo"),
      "No columns were selected in `remove_role[(][)]`"
    ),
    rec
  )
})

test_that('bad args', {
  expect_error(
    recipe(x = biomass) %>%
      add_role(carbon, new_role = letters[1:2]),
    "`new_role` must have length 1."
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
      prep(training = biomass) %>%
      juice() %>%
      ncol(),

    8
  )

  # second type
  expect_equal(
    rec %>%
      add_role(carbon, new_type = "carb") %>%
      prep(training = biomass) %>%
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
    prep(training = biomass)

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
    prep(training = biomass) %>%
    summary()

  expect_false("carbon" %in% rec_prepped$variable)
})

# ------------------------------------------------------------------------------
# Tests related to #296
# https://github.com/tidymodels/recipes/issues/296

test_that("Existing `NA` roles are not modified in prep() when new columns are generated", {

  rec_dummy <- recipe(x = iris) %>%
    update_role(Sepal.Length, new_role = "outcome") %>%
    update_role(Species, new_role = "predictor") %>%
    step_dummy(Species)

  prepped_rec_dummy <- prep(rec_dummy, iris)

  orig <- summary(rec_dummy)
  new <- summary(prepped_rec_dummy)

  # These should be identical except for the modified Species term
  expect_equal(
    filter(orig, !grepl("Species", variable)),
    filter(new, !grepl("Species", variable))
  )

  expect_equal(
    filter(new, grepl("Species", variable)),
    tibble(
      variable = c("Species_versicolor", "Species_virginica"),
      type = rep("numeric", times = 2),
      role = rep("predictor", times = 2),
      source = rep("derived", times = 2)
    )
  )

  # Juicing with all predictors should only give these two columns
  expect_equal(
    colnames(juice(prepped_rec_dummy, all_predictors())),
    c("Species_versicolor", "Species_virginica")
  )

})


test_that("Existing `NA` roles are not modified in prep() when multiple new columns are generated", {

  rec <- recipe(x = iris) %>%
    update_role(Sepal.Length, new_role = "outcome") %>%
    update_role(Sepal.Width, new_role = "predictor") %>%
    update_role(Species, new_role = "predictor") %>%
    step_dummy(Species) %>%
    step_bs(Sepal.Width)

  prepped_rec <- prep(rec, iris)

  orig <- summary(rec)
  new <- summary(prepped_rec)

  # These should be identical except for the
  # modified Species and Sepal.Width terms
  expect_equal(
    filter(orig, !grepl("Species", variable), !grepl("Sepal.Width", variable)),
    filter(new, !grepl("Species", variable), !grepl("Sepal.Width", variable))
  )

})
