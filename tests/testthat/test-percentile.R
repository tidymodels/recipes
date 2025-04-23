library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

test_that("simple percentile trans", {
  rec <- recipe(~., data = biomass_tr) |>
    step_percentile(carbon, sulfur)

  rec_trained <- prep(rec)
  biomass_tr_baked <- bake(rec_trained, new_data = biomass_tr)
  biomass_te_baked <- bake(rec_trained, new_data = biomass_te)

  carbon_quantiles <- quantile(
    biomass_tr$carbon,
    probs = (0:100) / 100,
    names = TRUE
  )
  sulfur_quantiles <- quantile(
    biomass_tr$sulfur,
    probs = (0:100) / 100,
    names = TRUE
  )
  sulfur_quantiles <- sulfur_quantiles[!duplicated(sulfur_quantiles)]

  expect_equal(
    approx(carbon_quantiles, y = 0:100, xout = biomass_tr$carbon)$y / 100,
    biomass_tr_baked$carbon
  )
  expect_equal(
    approx(carbon_quantiles, y = 0:100, xout = biomass_te$carbon)$y / 100,
    biomass_te_baked$carbon
  )
  sulfur_values <- as.numeric(gsub("%$", "", names(sulfur_quantiles)))
  expect_equal(
    approx(sulfur_quantiles, y = sulfur_values, xout = biomass_tr$sulfur)$y /
      100,
    biomass_tr_baked$sulfur
  )
  expect_equal(
    approx(sulfur_quantiles, y = sulfur_values, xout = biomass_te$sulfur)$y /
      100,
    biomass_te_baked$sulfur
  )
})

test_that("works works with fewer unique values than percentiles requested", {
  biomass_tr1 <- biomass_tr |>
    mutate(carbon1 = round(carbon, -1))
  biomass_te1 <- biomass_te |>
    mutate(carbon1 = round(carbon, -1))

  rec <- recipe(~., data = biomass_tr1) |>
    step_percentile(carbon1)

  rec_trained <- prep(rec)
  biomass_tr_baked <- bake(rec_trained, new_data = biomass_tr1)
  biomass_te_baked <- bake(rec_trained, new_data = biomass_te1)

  carbon1_quantiles <- quantile(
    biomass_tr1$carbon1,
    probs = (0:100) / 100,
    names = TRUE
  )
  carbon1_quantiles <- carbon1_quantiles[!duplicated(carbon1_quantiles)]

  carbon1_values <- as.numeric(gsub("%$", "", names(carbon1_quantiles)))
  expect_equal(
    approx(
      carbon1_quantiles,
      y = carbon1_values,
      xout = biomass_tr1$carbon1
    )$y /
      100,
    biomass_tr_baked$carbon1
  )
  expect_equal(
    approx(
      carbon1_quantiles,
      y = carbon1_values,
      xout = biomass_te1$carbon1
    )$y /
      100,
    biomass_te_baked$carbon1
  )
})

test_that("passing new probs works", {
  rec <- recipe(~., data = biomass_tr) |>
    step_percentile(carbon, sulfur, options = list(probs = seq(0, 1, by = 0.2)))

  rec_trained <- prep(rec)
  biomass_tr_baked <- bake(rec_trained, new_data = biomass_tr)
  biomass_te_baked <- bake(rec_trained, new_data = biomass_te)

  sulfur_quantiles <- quantile(
    biomass_tr$sulfur,
    probs = seq(0, 1, by = 0.2),
    names = TRUE
  )

  sulfur_values <- as.numeric(gsub("%$", "", names(sulfur_quantiles)))
  expect_equal(
    approx(sulfur_quantiles, y = sulfur_values, xout = biomass_tr$sulfur)$y /
      100,
    biomass_tr_baked$sulfur
  )
  expect_equal(
    approx(sulfur_quantiles, y = sulfur_values, xout = biomass_te$sulfur)$y /
      100,
    biomass_te_baked$sulfur
  )
})

test_that("outside argument", {
  train_df <- tibble(a = 1:9)

  new_df <- tibble(a = c(0.99, 5, 9.01))

  expect_identical(
    recipe(~a, data = train_df) |>
      step_percentile(a, outside = "none") |>
      prep() |>
      bake(new_data = new_df),
    tibble(a = c(NA, 0.5, NA))
  )

  expect_identical(
    recipe(~a, data = train_df) |>
      step_percentile(a, outside = "lower") |>
      prep() |>
      bake(new_data = new_df),
    tibble(a = c(0, 0.5, NA))
  )

  expect_identical(
    recipe(~a, data = train_df) |>
      step_percentile(a, outside = "upper") |>
      prep() |>
      bake(new_data = new_df),
    tibble(a = c(NA, 0.5, 1))
  )

  expect_identical(
    recipe(~a, data = train_df) |>
      step_percentile(a, outside = "both") |>
      prep() |>
      bake(new_data = new_df),
    tibble(a = c(0, 0.5, 1))
  )

  expect_snapshot(
    error = TRUE,
    recipe(~a, data = train_df) |>
      step_percentile(a, outside = "left") |>
      prep() |>
      bake(new_data = new_df)
  )
})

test_that("case weights", {
  test_wts <- rep(c(1, 0), c(200, 256))
  biomass_tr_cw <- biomass_tr |>
    mutate(wts = frequency_weights(test_wts))

  rec <- recipe(~., data = biomass_tr_cw) |>
    step_percentile(carbon, sulfur)

  rec_trained <- prep(rec)
  biomass_tr_baked <- bake(rec_trained, new_data = biomass_tr)
  biomass_te_baked <- bake(rec_trained, new_data = biomass_te)

  carbon_quantiles <- wrighted_quantile(
    biomass_tr$carbon[test_wts == 1],
    wts = rep(1, sum(test_wts)),
    probs = (0:100) / 100
  )
  sulfur_quantiles <- wrighted_quantile(
    biomass_tr$sulfur[test_wts == 1],
    wts = rep(1, sum(test_wts)),
    probs = (0:100) / 100
  )

  carbon_quantiles <- carbon_quantiles[!duplicated(carbon_quantiles)]
  sulfur_quantiles <- sulfur_quantiles[!duplicated(sulfur_quantiles)]

  expect_equal(
    rec_trained$steps[[1]]$ref_dist$carbon,
    carbon_quantiles
  )

  expect_equal(
    rec_trained$steps[[1]]$ref_dist$sulfur,
    sulfur_quantiles
  )

  expect_snapshot(rec_trained)

  # ----------------------------------------------------------------------------

  test_wts <- rep(c(1, 0), c(200, 256))
  biomass_tr_cw <- biomass_tr |>
    mutate(wts = importance_weights(test_wts))

  rec <- recipe(~., data = biomass_tr_cw) |>
    step_percentile(carbon, sulfur)

  rec_trained <- prep(rec)
  biomass_tr_baked <- bake(rec_trained, new_data = biomass_tr)
  biomass_te_baked <- bake(rec_trained, new_data = biomass_te)

  carbon_quantiles <- quantile(
    biomass_tr$carbon,
    probs = (0:100) / 100
  )
  sulfur_quantiles <- quantile(
    biomass_tr$sulfur,
    probs = (0:100) / 100
  )

  carbon_quantiles <- carbon_quantiles[!duplicated(carbon_quantiles)]
  sulfur_quantiles <- sulfur_quantiles[!duplicated(sulfur_quantiles)]

  expect_equal(
    rec_trained$steps[[1]]$ref_dist$carbon,
    carbon_quantiles
  )

  expect_equal(
    rec_trained$steps[[1]]$ref_dist$sulfur,
    sulfur_quantiles
  )

  expect_snapshot(rec_trained)
})

test_that("check_options() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(~mpg, data = mtcars) |>
      step_percentile(mpg, options = TRUE) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = biomass_tr) |>
    step_percentile(carbon, sulfur) |>
    update_role(carbon, sulfur, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = biomass_tr[, c(-3, -7)])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_percentile(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_percentile(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_percentile(rec)

  expect <- tibble(
    terms = character(),
    value = numeric(),
    percentile = numeric(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = biomass_tr) |>
    step_percentile(carbon, sulfur)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_percentile(disp, mpg) |>
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
