library(testthat)
library(recipes)
library(splines)

skip_if_not_installed("modeldata")

data(biomass, package = "modeldata")

# ------------------------------------------------------------------------------

test_that("correct convex functions", {
  skip_if_not_installed("splines2")
  library(splines2)

  biomass_tr <- biomass[biomass$dataset == "Training", ]
  biomass_te <- biomass[biomass$dataset == "Testing", ]

  rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
                data = biomass_tr
  )

  with_ns <- rec %>%
    step_spline_convex(carbon, hydrogen, deg_free = 5)

  with_ns <- prep(with_ns, training = biomass_tr, verbose = FALSE)

  with_ns_pred_tr <- bake(with_ns, new_data = biomass_tr)
  with_ns_pred_te <- bake(with_ns, new_data = biomass_te)

  carbon_ns_tr_exp <- cSpline(biomass_tr$carbon, df = 5)
  hydrogen_ns_tr_exp <- cSpline(biomass_tr$hydrogen, df = 5)
  carbon_ns_te_exp <- predict(carbon_ns_tr_exp, biomass_te$carbon)
  hydrogen_ns_te_exp <- predict(hydrogen_ns_tr_exp, biomass_te$hydrogen)

  expect_equal(
    unname(attr(carbon_ns_tr_exp, "knots")),
    with_ns$steps[[1]]$results$carbon$knots
  )
  expect_equal(
    unname(attr(carbon_ns_tr_exp, "Boundary.knots")),
    with_ns$steps[[1]]$results$carbon$Boundary.knots
  )
  expect_equal(
    unname(attr(hydrogen_ns_tr_exp, "knots")),
    with_ns$steps[[1]]$results$hydrogen$knots
  )
  expect_equal(
    unname(attr(hydrogen_ns_tr_exp, "Boundary.knots")),
    with_ns$steps[[1]]$results$hydrogen$Boundary.knots
  )

  carbon_ns_tr_res <- as.matrix(with_ns_pred_tr[, grep("carbon", names(with_ns_pred_tr))])
  colnames(carbon_ns_tr_res) <- NULL
  hydrogen_ns_tr_res <- as.matrix(with_ns_pred_tr[, grep("hydrogen", names(with_ns_pred_tr))])
  colnames(hydrogen_ns_tr_res) <- NULL

  carbon_ns_te_res <- as.matrix(with_ns_pred_te[, grep("carbon", names(with_ns_pred_te))])
  colnames(carbon_ns_te_res) <- 1:ncol(carbon_ns_te_res)
  hydrogen_ns_te_res <- as.matrix(with_ns_pred_te[, grep("hydrogen", names(with_ns_pred_te))])
  colnames(hydrogen_ns_te_res) <- 1:ncol(hydrogen_ns_te_res)

  ## remove attributes
  carbon_ns_tr_exp <- matrix(carbon_ns_tr_exp, ncol = 5)
  carbon_ns_te_exp <- matrix(carbon_ns_te_exp, ncol = 5)
  hydrogen_ns_tr_exp <- matrix(hydrogen_ns_tr_exp, ncol = 5)
  hydrogen_ns_te_exp <- matrix(hydrogen_ns_te_exp, ncol = 5)
  dimnames(carbon_ns_tr_res) <- NULL
  dimnames(carbon_ns_te_res) <- NULL
  dimnames(hydrogen_ns_tr_res) <- NULL
  dimnames(hydrogen_ns_te_res) <- NULL

  expect_equal(carbon_ns_tr_res, carbon_ns_tr_exp)
  expect_equal(carbon_ns_te_res, carbon_ns_te_exp)
  expect_equal(hydrogen_ns_tr_res, hydrogen_ns_tr_exp)
  expect_equal(hydrogen_ns_te_res, hydrogen_ns_te_exp)
})


test_that("printing", {

  biomass_tr <- biomass[biomass$dataset == "Training", ]
  biomass_te <- biomass[biomass$dataset == "Testing", ]

  rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
                data = biomass_tr
  )

  with_ns <- rec %>% step_spline_convex(carbon, hydrogen)
  expect_snapshot(print(with_ns))
  expect_snapshot(prep(with_ns))
})


test_that("tunable", {

  biomass_tr <- biomass[biomass$dataset == "Training", ]
  biomass_te <- biomass[biomass$dataset == "Testing", ]

  rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
                data = biomass_tr
  )

  rec <-
    recipe(~., data = iris) %>%
    step_spline_convex(all_predictors())
  rec_param <- tunable.step_spline_convex(rec$steps[[1]])
  expect_equal(rec_param$name, c("deg_free", "degree"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_spline_convex(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_spline_convex(rec)

  expect <- tibble(terms = "<none>", id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec_prepped <- prep(rec, mtcars)
  expect_prepped <- tibble::tibble(terms = "<none>", id = rec_prepped$steps[[1]]$id)
  expect_identical(tidy(rec_prepped, number = 1), expect_prepped)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_spline_convex(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
