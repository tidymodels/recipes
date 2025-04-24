library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

test_that("default method", {
  rec <- recipe(x = biomass)
  exp_res <- tibble(
    variable = colnames(biomass),
    type = rep(
      list(c("string", "unordered", "nominal"), c("double", "numeric")),
      c(2, 6)
    ),
    role = NA_character_,
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("changing roles", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "some other role")
  exp_res <- tibble(
    variable = colnames(biomass),
    type = rep(
      list(c("string", "unordered", "nominal"), c("double", "numeric")),
      c(2, 6)
    ),
    role = rep(c("some other role", NA), c(1, 7)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("change existing role", {
  rec <- recipe(x = biomass)

  expect_snapshot(
    error = TRUE,
    add_role(rec, sample, new_role = "some other role")
  )

  rec <- update_role(rec, sample, new_role = "some other role")
  rec <- update_role(rec, sample, new_role = "other other role")

  exp_res <- tibble(
    variable = colnames(biomass),
    type = rep(
      list(c("string", "unordered", "nominal"), c("double", "numeric")),
      c(2, 6)
    ),
    role = rep(c("other other role", NA), c(1, 7)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("change only 1 role of variable with multiple roles", {
  rec <- recipe(x = biomass)
  rec <-
    rec |>
    update_role(sample, new_role = "role 1") |>
    add_role(sample, new_role = "role 2")

  orig_roles <- rec

  # changes only 1
  rec <- update_role(rec, sample, new_role = "role 3", old_role = "role 1")

  exp_res <- summary(orig_roles)
  exp_res$role[exp_res$role == "role 1"] <- "role 3"
  exp_res$required_to_bake <- TRUE
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("change every role of 2 variables", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, dataset, new_role = "role 1")
  orig_roles <- summary(rec)
  rec <- update_role(rec, sample, dataset, new_role = "role 2")

  exp_res <- orig_roles
  exp_res$role[exp_res$role == "role 1"] <- "role 2"
  exp_res$required_to_bake <- TRUE

  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("update only NA role", {
  rec <- recipe(x = biomass)
  orig_rec <- summary(rec)
  rec <- update_role(rec, sample, dataset, new_role = "some other role")

  exp_res <- orig_rec |> arrange(variable)
  exp_res$role[
    exp_res$variable %in% c("sample", "dataset")
  ] <- "some other role"
  exp_res$required_to_bake <- TRUE

  expect_equal(summary(rec, TRUE) |> arrange(variable), exp_res)
})

test_that("new role for existing NA role", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "some other role")

  exp_res <- tibble(
    variable = colnames(biomass),
    type = rep(
      list(c("string", "unordered", "nominal"), c("double", "numeric")),
      c(2, 6)
    ),
    role = rep(c("some other role", NA), c(1, length(colnames(biomass)) - 1)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("new role with specified type", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(
    rec,
    sample,
    new_role = "some other role",
    new_type = "new type"
  )

  exp_res <- tibble(
    variable = c("sample", colnames(biomass)),
    type = c(
      list(c("string", "unordered", "nominal")),
      list("new type"),
      list(c("string", "unordered", "nominal")),
      rep(list(c("double", "numeric")), 6)
    ),
    role = rep(c("blah", "some other role", NA), c(1, 1, 7)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("add new role when two already exist with different types", {
  # type of the first existing role found is used
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(
    rec,
    sample,
    new_role = "some other role",
    new_type = "new type"
  )
  rec <- add_role(rec, sample, new_role = "another role")

  exp_res <- tibble(
    variable = c("sample", "sample", colnames(biomass)),
    type = c(
      list(c("string", "unordered", "nominal")),
      list("new type"),
      list(c("string", "unordered", "nominal")),
      list(c("string", "unordered", "nominal")),
      rep(list(c("double", "numeric")), 6)
    ),
    role = c("blah", "some other role", "another role", rep(NA, 7)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("existing role is skipped", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(rec, sample, new_role = "some other role")

  # skip me
  expect_snapshot(
    rec <- add_role(rec, sample, new_role = "some other role")
  )

  # also tests the order, new roles come directly after old ones
  exp_res <- tibble(
    variable = c("sample", colnames(biomass)),
    type = rep(
      list(c("string", "unordered", "nominal"), c("double", "numeric")),
      c(3, 6)
    ),
    role = rep(c("blah", "some other role", NA), c(1, 1, 7)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("existing role is skipped, but new one is added", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "blah")
  rec <- add_role(rec, sample, new_role = "some other role")

  # partially skip me
  expect_snapshot(
    rec <- add_role(rec, sample, dataset, new_role = "some other role")
  )

  exp_res <- tibble(
    variable = c(
      rep(c("sample", "dataset"), c(2, 2)),
      setdiff(colnames(biomass), c("sample", "dataset"))
    ),
    type = rep(
      list(c("string", "unordered", "nominal"), c("double", "numeric")),
      c(4, 6)
    ),
    role = c("blah", "some other role", NA, "some other role", rep(NA, 6)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("cannot add roles if the current one is `NA`", {
  rec <- recipe(x = biomass)
  expect_snapshot(error = TRUE, add_role(rec, sample, sulfur))
})

test_that("`update_role()` cannot be ambiguous", {
  rec <- recipe(HHV ~ ., data = biomass)
  rec <- add_role(rec, sample, new_role = "x")

  expect_snapshot(error = TRUE, update_role(rec, sample, new_role = "y"))
})

test_that("`new_role` cannot be `NA_character_`", {
  rec <- recipe(x = biomass)

  expect_snapshot(error = TRUE, add_role(rec, sample, new_role = NA_character_))

  expect_snapshot(
    error = TRUE,
    update_role(rec, sample, new_role = NA_character_)
  )
})

test_that("remove roles", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "role1")
  expect_snapshot(error = TRUE, rec <- remove_role(rec, sample, old_role = NA))
  expect_snapshot(error = TRUE, rec <- remove_role(rec, sample))

  expect_snapshot(
    remove_role(rec, sample, old_role = "non-existant")
  )

  rec <- remove_role(rec, sample, old_role = "role1")

  exp_res <- tibble(
    variable = colnames(biomass),
    type = rep(
      list(c("string", "unordered", "nominal"), c("double", "numeric")),
      c(2, 6)
    ),
    role = NA_character_,
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("New type for an existing role can be added", {
  rec <- recipe(x = biomass)
  rec <- update_role(rec, sample, new_role = "role1")
  rec <- add_role(rec, sample, new_role = "role1", new_type = "text")

  exp_res <- tibble(
    variable = c("sample", colnames(biomass)),
    type = c(
      list(c("string", "unordered", "nominal")),
      list("text"),
      list(c("string", "unordered", "nominal")),
      rep(list(c("double", "numeric")), 6)
    ),
    role = c("role1", "role1", rep(NA, 7)),
    source = "original",
    required_to_bake = TRUE
  )
  expect_equal(summary(rec, TRUE), exp_res)
})

test_that("can use tidyselect ops in role selection", {
  rec <- recipe(mpg ~ ., mtcars) |>
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

  expect_snapshot(
    rec2 <- add_role(rec)
  )
  expect_identical(rec2, rec)

  expect_snapshot(
    rec2 <- update_role(rec)
  )
  expect_identical(rec2, rec)

  expect_snapshot(
    rec2 <- remove_role(rec, old_role = "foo")
  )
  expect_identical(rec2, rec)

  expect_snapshot(
    rec2 <- add_role(rec, starts_with("foobar"))
  )
  expect_identical(rec2, rec)

  expect_snapshot(
    rec2 <- update_role(rec, starts_with("foobar"))
  )
  expect_identical(rec2, rec)

  expect_snapshot(
    rec2 <- remove_role(rec, starts_with("foobar"), old_role = "foo")
  )
  expect_identical(rec2, rec)
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(x = biomass) |>
      add_role(carbon, new_role = letters[1:2])
  )

  expect_snapshot(
    error = TRUE,
    recipe(x = biomass) |>
      add_role(carbon, new_role = "a", new_type = letters[1:2])
  )

  expect_snapshot(
    error = TRUE,
    recipe(x = biomass) |>
      update_role(carbon, new_role = c("a", "b"))
  )

  expect_snapshot(
    error = TRUE,
    recipe(x = biomass) |>
      update_role(carbon, old_role = c("a", "b"))
  )
})

# ------------------------------------------------------------------------------
# Multiples roles + Selection testing

test_that("adding multiple roles/types does not duplicate prepped columns", {
  rec <- recipe(HHV ~ ., data = biomass)

  # second role
  expect_equal(
    rec |>
      add_role(carbon, new_role = "carb") |>
      prep(training = biomass) |>
      bake(new_data = NULL) |>
      ncol(),
    8
  )

  # second type
  expect_equal(
    rec |>
      add_role(carbon, new_type = "carb") |>
      prep(training = biomass) |>
      bake(new_data = NULL) |>
      ncol(),
    8
  )
})

test_that("type selectors can be combined", {
  rec <- recipe(HHV ~ ., data = biomass)

  prepped <- rec |>
    add_role(carbon, new_role = "predictor", new_type = "carb") |>
    step_center(all_numeric(), -has_type("carb")) |>
    prep(training = biomass)

  expect_equal(
    names(prepped$steps[[1]]$means),
    c("hydrogen", "oxygen", "nitrogen", "sulfur", "HHV")
  )
})

test_that("step_rm() removes ALL mention of variables with that role", {
  rec <- recipe(HHV ~ ., data = biomass)

  rec_prepped <- rec |>
    add_role(carbon, new_role = "predictor", new_type = "carb") |>
    step_rm(has_type("carb")) |>
    prep(training = biomass) |>
    summary()

  expect_false("carbon" %in% rec_prepped$variable)
})

# ------------------------------------------------------------------------------
# Tests related to #296
# https://github.com/tidymodels/recipes/issues/296

test_that("Existing `NA` roles are not modified in prep() when new columns are generated", {
  rec_dummy <- recipe(x = iris) |>
    update_role(Sepal.Length, new_role = "outcome") |>
    update_role(Species, new_role = "predictor") |>
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
      type = rep(list(c("double", "numeric")), 2),
      role = rep("predictor", times = 2),
      source = rep("derived", times = 2)
    )
  )

  # Juicing with all predictors should only give these two columns
  expect_equal(
    colnames(bake(prepped_rec_dummy, new_data = NULL, all_predictors())),
    c("Species_versicolor", "Species_virginica")
  )
})

test_that("Existing `NA` roles are not modified in prep() when multiple new columns are generated", {
  rec <- recipe(x = iris) |>
    update_role(Sepal.Length, new_role = "outcome") |>
    update_role(Sepal.Width, new_role = "predictor") |>
    update_role(Species, new_role = "predictor") |>
    step_dummy(Species) |>
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

test_that("Roles are correcly selected in bake", {
  x <- tibble::tibble(
    a = runif(10),
    b = runif(10),
    c = runif(10)
  )

  rec <- recipe(c ~ ., x) |>
    update_role(b, new_role = "id") |>
    add_role(a, new_role = "id") |>
    prep()

  o <- recipes::bake(rec, x, recipes::has_role("id"))
  expect_equal(names(o), c("a", "b"))
})

test_that("role functions handle case weights correctly", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      update_role("disp", new_role = "case_weights")
  )

  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      add_role("disp", new_role = "case_weights")
  )

  mtcars1 <- mtcars |>
    mutate(wt = importance_weights(wt))

  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars1) |>
      remove_role(wt, old_role = "case_weights")
  )

  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars1) |>
      update_role(wt)
  )

  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars1) |>
      add_role(wt)
  )
})

test_that("role information from summary()", {
  # ----------------------------------------------------------------------------
  # non-formula method

  rec_roles <- recipe(mtcars) |>
    update_role(-mpg, new_role = "predictor") |>
    update_role(mpg, new_role = "outcome") |>
    update_role(gear, new_role = "id") |>
    update_role(carb, new_role = "important") |>
    prep()

  req_roles <-
    rec_roles |>
    update_role_requirements("important", bake = FALSE) |>
    prep()

  expect_snapshot(summary(rec_roles, original = TRUE))
  expect_snapshot(summary(req_roles, original = TRUE))

  # ----------------------------------------------------------------------------
  # missing role values

  na_rec <-
    mtcars |>
    recipe() |>
    update_role(mpg, new_role = "outcome") |>
    update_role(disp, wt, new_role = "predictor") |>
    update_role(carb, new_role = "other") |>
    prep()

  na_req_rec <-
    na_rec |>
    update_role_requirements("NA", bake = FALSE) |>
    prep()

  expect_snapshot(summary(na_rec, original = TRUE))
  expect_snapshot(summary(na_req_rec, original = TRUE))
})

test_that("add_roles() error if columns would be both predictor and outcome", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      add_role(mpg, new_role = "predictor")
  )

  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      add_role(disp, new_role = "outcome")
  )
})
