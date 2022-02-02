library(testthat)
library(recipes)
library(modeldata)
data(biomass)
library(splines)


biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

test_that('correct basis functions', {
  with_bs <- rec %>%
    step_bs(carbon, hydrogen, deg_free = 5, degree = 2)

  with_bs <- prep(with_bs, training = biomass_tr, verbose = FALSE)

  with_bs_pred_tr <- bake(with_bs, new_data = biomass_tr)
  with_bs_pred_te <- bake(with_bs, new_data = biomass_te)

  carbon_bs_tr_exp <- bs(biomass_tr$carbon, df = 5, degree = 2)
  hydrogen_bs_tr_exp <- bs(biomass_tr$hydrogen, df = 5, degree = 2)
  carbon_bs_te_exp <- predict(carbon_bs_tr_exp, biomass_te$carbon)
  hydrogen_bs_te_exp <- predict(hydrogen_bs_tr_exp, biomass_te$hydrogen)

  expect_equal(
    unname(attr(carbon_bs_tr_exp, "knots")),
    attr(with_bs$steps[[1]]$objects$carbon, "knots")
  )
  expect_equal(
    unname(attr(carbon_bs_tr_exp, "Boundary.knots")),
    attr(with_bs$steps[[1]]$objects$carbon, "Boundary.knots")
  )
  expect_equal(
    unname(attr(hydrogen_bs_tr_exp, "knots")),
    attr(with_bs$steps[[1]]$objects$hydrogen, "knots")
  )
  expect_equal(
    unname(attr(hydrogen_bs_tr_exp, "Boundary.knots")),
    attr(with_bs$steps[[1]]$objects$hydrogen, "Boundary.knots")
  )

  carbon_bs_tr_res <- as.matrix(with_bs_pred_tr[, grep("carbon", names(with_bs_pred_tr))])
  colnames(carbon_bs_tr_res) <- NULL
  hydrogen_bs_tr_res <- as.matrix(with_bs_pred_tr[, grep("hydrogen", names(with_bs_pred_tr))])
  colnames(hydrogen_bs_tr_res) <- NULL

  carbon_bs_te_res <- as.matrix(with_bs_pred_te[, grep("carbon", names(with_bs_pred_te))])
  colnames(carbon_bs_te_res) <- 1:ncol(carbon_bs_te_res)
  hydrogen_bs_te_res <- as.matrix(with_bs_pred_te[, grep("hydrogen", names(with_bs_pred_te))])
  colnames(hydrogen_bs_te_res) <- 1:ncol(hydrogen_bs_te_res)

  ## remove attributes
  carbon_bs_tr_exp <- matrix(carbon_bs_tr_exp, ncol = 5)
  carbon_bs_te_exp <- matrix(carbon_bs_te_exp, ncol = 5)
  hydrogen_bs_tr_exp <- matrix(hydrogen_bs_tr_exp, ncol = 5)
  hydrogen_bs_te_exp <- matrix(hydrogen_bs_te_exp, ncol = 5)
  dimnames(carbon_bs_tr_res) <- NULL
  dimnames(carbon_bs_te_res) <- NULL
  dimnames(hydrogen_bs_tr_res) <- NULL
  dimnames(hydrogen_bs_te_res) <- NULL

  expect_equal(carbon_bs_tr_res, carbon_bs_tr_exp)
  expect_equal(carbon_bs_te_res, carbon_bs_te_exp)
  expect_equal(hydrogen_bs_tr_res, hydrogen_bs_tr_exp)
  expect_equal(hydrogen_bs_te_res, hydrogen_bs_te_exp)
})


test_that('printing', {
  with_bs <- rec %>%  step_bs(carbon, hydrogen)
  expect_output(print(with_bs))
  expect_output(prep(with_bs, training = biomass_tr, verbose = TRUE))
})


test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_bs(all_predictors())
  rec_param <- tunable.step_bs(rec$steps[[1]])
  expect_equal(rec_param$name, c("deg_free", "degree"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_bs(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_bs(rec)

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
  rec <- step_bs(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

