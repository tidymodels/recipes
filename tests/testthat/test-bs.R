library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

test_that("correct basis functions", {
  with_bs <- rec |>
    step_bs(carbon, hydrogen, deg_free = 5, degree = 2)

  with_bs <- prep(with_bs, training = biomass_tr, verbose = FALSE)

  with_bs_pred_tr <- bake(with_bs, new_data = biomass_tr)
  with_bs_pred_te <- bake(with_bs, new_data = biomass_te)

  carbon_bs_tr_exp <- splines::bs(biomass_tr$carbon, df = 5, degree = 2)
  hydrogen_bs_tr_exp <- splines::bs(biomass_tr$hydrogen, df = 5, degree = 2)
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

  carbon_bs_tr_res <- as.matrix(
    with_bs_pred_tr[, grep("carbon", names(with_bs_pred_tr))]
  )
  colnames(carbon_bs_tr_res) <- NULL
  hydrogen_bs_tr_res <- as.matrix(
    with_bs_pred_tr[, grep("hydrogen", names(with_bs_pred_tr))]
  )
  colnames(hydrogen_bs_tr_res) <- NULL

  carbon_bs_te_res <- as.matrix(
    with_bs_pred_te[, grep("carbon", names(with_bs_pred_te))]
  )
  colnames(carbon_bs_te_res) <- 1:ncol(carbon_bs_te_res)
  hydrogen_bs_te_res <- as.matrix(
    with_bs_pred_te[, grep("hydrogen", names(with_bs_pred_te))]
  )
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

test_that("options(knots) works correctly (#1297)", {
  exmaple_data <- tibble(x = seq(-2, 2, 0.01))

  rec_res <- recipe(~., data = exmaple_data) |>
    step_bs(
      x,
      options = list(knots = seq(-1, 1, 0.125), Boundary.knots = c(-2.5, 2.5))
    ) |>
    prep() |>
    bake(new_data = NULL)

  mm_res <- model.matrix(
    ~ splines::bs(
      x,
      knots = seq(-1, 1, 0.125),
      Boundary.knots = c(-2.5, 2.5)
    ) -
      1,
    data = exmaple_data
  )

  attr(mm_res, "assign") <- NULL
  mm_res <- setNames(as_tibble(mm_res), names(rec_res))

  expect_identical(rec_res, mm_res)
})

test_that("check_name() is used", {
  dat <- mtcars
  dat$mpg_bs_1 <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_bs(mpg)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_bs(all_predictors())
  rec_param <- tunable.step_bs(rec$steps[[1]])
  expect_equal(rec_param$name, c("deg_free", "degree"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("works when baked with 1 row", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_bs(disp) |>
    prep()

  expect_no_error(
    res <- bake(rec, mtcars[1, ])
  )

  expect_identical(nrow(res), 1L)
})

test_that("check_options() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_bs(disp, options = TRUE) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  with_bs <- rec |>
    step_bs(carbon, hydrogen, deg_free = 5, degree = 2) |>
    update_role(carbon, hydrogen, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  with_bs <- prep(with_bs, training = biomass_tr, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(with_bs, new_data = biomass_tr[, c(-4)]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_bs(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("mpg_bs_1", "mpg_bs_2", "mpg_bs_3")

  rec <- recipe(~mpg, mtcars) |>
    step_bs(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~mpg, mtcars) |>
    step_bs(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("mpg", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~mpg, mtcars) |>
    step_bs(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = mtcars)
  )
})

test_that("printing", {
  with_bs <- rec |>
    step_bs(carbon, hydrogen)

  expect_snapshot(print(with_bs))
  expect_snapshot(prep(with_bs))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_bs(
      all_predictors(),
      deg_free = hardhat::tune(),
      degree = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_bs(mpg, disp) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
