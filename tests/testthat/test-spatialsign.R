library(testthat)
library(recipes)
skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass
)

test_that("spatial sign", {
  sp_sign <- rec |>
    step_center(carbon, hydrogen) |>
    step_scale(carbon, hydrogen) |>
    step_spatialsign(carbon, hydrogen)

  sp_sign_trained <- prep(sp_sign, training = biomass, verbose = FALSE)

  sp_sign_pred <- bake(sp_sign_trained, new_data = biomass)
  sp_sign_pred <- as.matrix(sp_sign_pred)[, c("carbon", "hydrogen")]

  x <- as.matrix(scale(biomass[, 3:4], center = TRUE, scale = TRUE))
  x <- t(apply(x, 1, function(x) x / sqrt(sum(x^2))))

  expect_equal(sp_sign_pred, x)

  expect_snapshot(
    rec |>
      step_spatialsign(carbon, hydrogen, na_rm = 12) |>
      prep(),
    error = TRUE
  )
})

test_that("Missing values", {
  with_na <- head(biomass)
  with_na$carbon[1] <- NA
  with_na$hydrogen[2] <- NA
  rownames(with_na) <- NULL

  sp_sign_rm_na <- rec |>
    step_spatialsign(carbon, hydrogen) |>
    prep() |>
    bake(
      new_data = with_na,
      one_of(c("carbon", "hydrogen")),
      composition = "matrix"
    )

  sp_sign_no_rm_na <- rec |>
    step_spatialsign(carbon, hydrogen, na_rm = FALSE) |>
    prep() |>
    bake(
      new_data = with_na,
      one_of(c("carbon", "hydrogen")),
      composition = "matrix"
    )

  x <- as.matrix(with_na[, 3:4])
  exp_rm_na <- t(apply(x, 1, function(x) x / sqrt(sum(x^2, na.rm = TRUE))))
  exp_no_rm_na <- t(apply(x, 1, function(x) x / sqrt(sum(x^2, na.rm = FALSE))))

  expect_equal(sp_sign_rm_na, exp_rm_na)
  expect_equal(sp_sign_no_rm_na, exp_no_rm_na)
})

test_that("centering with case weights", {
  mtcars_freq <- mtcars
  mtcars_freq$cyl <- frequency_weights(mtcars_freq$cyl)

  rec <-
    recipe(mpg ~ ., mtcars_freq) |>
    step_spatialsign(all_numeric_predictors()) |>
    prep()

  expect_equal(
    rowSums(bake(rec, new_data = NULL, -c(cyl, mpg))^2),
    as.numeric(mtcars_freq$cyl)
  )

  expect_snapshot(rec)

  # ----------------------------------------------------------------------------

  mtcars_imp <- mtcars
  mtcars_imp$wt <- importance_weights(mtcars_imp$wt)

  rec <-
    recipe(mpg ~ ., mtcars_imp) |>
    step_spatialsign(all_numeric_predictors()) |>
    prep()

  expect_equal(
    rowSums(bake(rec, new_data = NULL, -c(wt, mpg))^2),
    rep(1, nrow(mtcars_imp))
  )

  expect_snapshot(rec)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  sp_sign <- rec |>
    step_spatialsign(carbon, hydrogen) |>
    update_role(carbon, hydrogen, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  sp_sign_trained <- prep(sp_sign, training = biomass, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(sp_sign_trained, new_data = biomass[, c(-3)])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_spatialsign(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_spatialsign(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_spatialsign(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(
    HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
    data = biomass
  ) |>
    step_center(carbon, hydrogen) |>
    step_scale(carbon, hydrogen) |>
    step_spatialsign(carbon, hydrogen)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_spatialsign(all_predictors()) |>
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
