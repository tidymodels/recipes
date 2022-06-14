library(testthat)
library(recipes)
library(dplyr)
library(hardhat)

rp1 <- recipe(mtcars, cyl ~ .)
rp2 <- recipe(mtcars, cyl ~ mpg + drat)

test_that("check_col works in the prep stage", {
  expect_error(rp1 %>% check_cols(everything()) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(everything()) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(cyl, mpg) %>% prep(), NA)
})


test_that("check_col works in the bake stage", {

  expect_error(rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars),
               NA)
  expect_equal(rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars),
               tibble(mtcars[ ,c(1, 3:11, 2)]))
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars), NA)
  expect_equal(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars),
               tibble(mtcars[ ,c(1, 5, 2)]))

  expect_error(
    rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars),
    NA
  )
  expect_equal(
    rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars),
    tibble(mtcars[, c(1, 3:11, 2)])
  )
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep() %>% bake(mtcars), NA)
  expect_equal(
    rp2 %>% check_cols(cyl, mpg, drat) %>% prep() %>% bake(mtcars),
    tibble(mtcars[, c(1, 5, 2)])
  )
  expect_snapshot(error = TRUE,
    rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars[-1])
  )
  expect_snapshot(error = TRUE,
    rp2 %>% check_cols(cyl, mpg, drat) %>% prep() %>%
      bake(mtcars[, c(2, 5)])
  )
})

test_that("printing", {
  rec <- rp1 %>%
    check_cols(everything())
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_cols(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_cols(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_cols(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("non-standard roles during bake/predict", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if(packageVersion("workflows") < "0.2.6.9001")
  skip_if(packageVersion("parsnip") < "1.0.0")

  # ----------------------------------------------------------------------------

  library(workflows)
  library(parsnip)

  # ----------------------------------------------------------------------------

  data(Chicago, package = "modeldata")

  Chicago <- Chicago %>% select(ridership, date, Austin, Belmont)

  set.seed(1)
  Chicago$wts <- importance_weights(runif(nrow(Chicago)))

  # ----------------------------------------------------------------------------

  base_wflow <-
    workflow() %>%
    add_model(linear_reg())

  # ----------------------------------------------------------------------------
  # non-standard roles, no additional blueprint

  ## no case weights, default blueprint
  role_rec <-
    recipe(ridership ~ date + Austin + Belmont, data = Chicago) %>%
    step_date(date) %>%
    update_role(date, new_role = "date")

  role_wflow <-
    base_wflow %>%
    add_recipe(role_rec)

  role_fit <- fit(role_wflow, data = Chicago)

  expect_snapshot_error(predict(role_fit, head(Chicago)))

  # ----------------------------------------------------------------------------
  # non-standard roles, additional blueprint

  bp <- default_recipe_blueprint(bake_dependent_roles = "date")

  role_bp_wflow <-
    base_wflow %>%
    add_recipe(role_rec, blueprint = bp)

  role_bp_fit <- fit(role_bp_wflow, data = Chicago)

  # This should require 'date' to predict
  expect_snapshot_error(predict(role_bp_fit, Chicago %>% select(-date)))
  expect_error(predict(role_bp_fit, head(Chicago)), regexp = NA)

  # ----------------------------------------------------------------------------
  # non-standard roles, case weights, additional blueprint

  role_wts_rec <-
    recipe(ridership ~ ., data = Chicago) %>%
    step_date(date) %>%
    update_role(date, new_role = "date")

  role_wts_wflow <-
    base_wflow %>%
    add_recipe(role_rec, blueprint = bp) %>%
    add_case_weights(wts)

  # TODO add tests here

  # ----------------------------------------------------------------------------
  # Removing variable after use

  rm_rec <-
    recipe(ridership ~ date + Austin + Belmont, data = Chicago) %>%
    step_date(date, keep_original_cols = FALSE)

  rm_wflow <-
    base_wflow %>%
    add_recipe(rm_rec)

  rm_fit <- fit(rm_wflow, data = Chicago)

  # This should require 'date' to predict
  expect_snapshot_error(predict(rm_fit, Chicago %>% select(-date)))

  # ----------------------------------------------------------------------------
  # Removing variable after use, with case weights

  rm_wts_rec <-
    recipe(ridership ~ ., data = Chicago) %>%
    step_date(date, keep_original_cols = FALSE)

  rm_wts_wflow <-
    base_wflow %>%
    add_recipe(rm_wts_rec) %>%
    add_case_weights(wts)

  rm_wts_fit <- fit(rm_wts_wflow, data = Chicago)

  # This should require 'date' but not 'wts' to predict
  expect_snapshot_error(predict(rm_fit, Chicago %>% select(-date)))

})


