library(testthat)
library(recipes)

dummies <- cbind(
  model.matrix(~ block - 1, npk),
  model.matrix(~ N - 1, npk),
  model.matrix(~ P - 1, npk),
  model.matrix(~ K - 1, npk),
  yield = npk$yield
)

dummies <- as.data.frame(dummies)

dum_rec <- recipe(yield ~ ., data = dummies)

###################################################################

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")
biomass$new_1 <- with(
  biomass,
  .1 * carbon - .2 * hydrogen + .6 * sulfur
)
biomass$new_2 <- with(
  biomass,
  .5 * carbon - .2 * oxygen + .6 * nitrogen
)

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

biomass_rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + new_1 + new_2,
  data = biomass_tr
)

###################################################################

test_that("example 1", {
  dum_filtered <- dum_rec |>
    step_lincomb(all_predictors())
  dum_filtered <- prep(dum_filtered, training = dummies, verbose = FALSE)
  removed <- c("N1", "P1", "K1")
  expect_equal(dum_filtered$steps[[1]]$removals, removed)
})

test_that("example 2", {
  lincomb_filter <- biomass_rec |>
    step_lincomb(all_predictors())

  filtering_trained <- prep(lincomb_filter, training = biomass_tr)
  test_res <- bake(filtering_trained, new_data = biomass_te, all_predictors())

  expect_true(all(!(paste0("new_", 1:2) %in% colnames(test_res))))
})

test_that("no exclusions", {
  biomass_rec_2 <- recipe(HHV ~ carbon + hydrogen, data = biomass_tr)
  lincomb_filter_2 <- biomass_rec_2 |>
    step_lincomb(all_predictors())

  filtering_trained_2 <- prep(lincomb_filter_2, training = biomass_tr)
  test_res_2 <- bake(
    filtering_trained_2,
    new_data = biomass_te,
    all_predictors()
  )

  expect_true(length(filtering_trained_2$steps[[1]]$removals) == 0)
  expect_true(all(colnames(test_res_2) == c("carbon", "hydrogen")))
})

test_that("doesn't remove both variables if identical (#1357)", {
  mtcars <- as_tibble(mtcars[1])
  exp <- mtcars
  mtcars$mpg_copy <- mtcars$mpg

  res <- recipe(~., data = mtcars) |>
    step_lincomb(all_numeric_predictors()) |>
    prep() |>
    bake(NULL)

  expect_identical(res, exp)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_lincomb() removes variables and thus does not care if they are not
  # there.
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lincomb(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_lincomb(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lincomb(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(yield ~ ., data = dummies) |>
    step_lincomb(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    dum_rec |>
      step_lincomb(all_predictors(), max_steps = 0) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_lincomb(all_numeric_predictors()) |>
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
