library(testthat)
library(recipes)
skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

biomass_tr <- biomass[1:10, ]
biomass_te <- biomass[c(13:14, 19, 522), ]

rec <- recipe(HHV ~ carbon + hydrogen, data = biomass_tr)

test_that("correct values", {
  standardized <- rec |>
    step_range(carbon, hydrogen, min = -12, id = "")

  standardized_trained <- prep(
    standardized,
    training = biomass_tr,
    verbose = FALSE
  )

  obs_pred <- bake(
    standardized_trained,
    new_data = biomass_te,
    all_predictors()
  )
  obs_pred <- as.matrix(obs_pred)

  mins <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, min)
  maxs <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, max)

  new_min <- -12
  new_max <- 1
  new_range <- new_max - new_min

  carb <- ((new_range * (biomass_te$carbon - mins["carbon"])) /
    (maxs["carbon"] - mins["carbon"])) +
    new_min
  carb <- ifelse(carb > new_max, new_max, carb)
  carb <- ifelse(carb < new_min, new_min, carb)

  hydro <- ((new_range * (biomass_te$hydrogen - mins["hydrogen"])) /
    (maxs["hydrogen"] - mins["hydrogen"])) +
    new_min
  hydro <- ifelse(hydro > new_max, new_max, hydro)
  hydro <- ifelse(hydro < new_min, new_min, hydro)

  exp_pred <- cbind(carb, hydro)
  colnames(exp_pred) <- c("carbon", "hydrogen")
  expect_equal(exp_pred, obs_pred)

  rng_tibble_un <-
    tibble(
      terms = c("carbon", "hydrogen"),
      min = rep(NA_real_, 2),
      max = rep(NA_real_, 2),
      id = ""
    )
  rng_tibble_tr <-
    tibble(
      terms = c("carbon", "hydrogen"),
      min = unname(mins),
      max = unname(maxs),
      id = ""
    )

  expect_equal(tidy(standardized, 1), rng_tibble_un)
  expect_equal(tidy(standardized_trained, 1), rng_tibble_tr)
})

test_that("defaults", {
  standardized <- rec |>
    step_range(carbon, hydrogen)

  standardized_trained <- prep(
    standardized,
    training = biomass_tr,
    verbose = FALSE
  )

  obs_pred <- bake(
    standardized_trained,
    new_data = biomass_te,
    all_predictors()
  )
  obs_pred <- as.matrix(obs_pred)

  mins <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, min)
  maxs <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, max)

  new_min <- 0
  new_max <- 1
  new_range <- new_max - new_min

  carb <- ((new_range * (biomass_te$carbon - mins["carbon"])) /
    (maxs["carbon"] - mins["carbon"])) +
    new_min
  carb <- ifelse(carb > new_max, new_max, carb)
  carb <- ifelse(carb < new_min, new_min, carb)

  hydro <- ((new_range * (biomass_te$hydrogen - mins["hydrogen"])) /
    (maxs["hydrogen"] - mins["hydrogen"])) +
    new_min
  hydro <- ifelse(hydro > new_max, new_max, hydro)
  hydro <- ifelse(hydro < new_min, new_min, hydro)

  exp_pred <- cbind(carb, hydro)
  colnames(exp_pred) <- c("carbon", "hydrogen")
  expect_equal(exp_pred, obs_pred)
})

test_that("one variable", {
  standardized <- rec |>
    step_range(carbon)

  standardized_trained <- prep(
    standardized,
    training = biomass_tr,
    verbose = FALSE
  )

  obs_pred <- bake(standardized_trained, new_data = biomass_te)

  mins <- min(biomass_tr$carbon)
  maxs <- max(biomass_tr$carbon)

  new_min <- 0
  new_max <- 1
  new_range <- new_max - new_min

  carb <- ((new_range * (biomass_te$carbon - mins)) /
    (maxs - mins)) +
    new_min
  carb <- ifelse(carb > new_max, new_max, carb)
  carb <- ifelse(carb < new_min, new_min, carb)

  expect_equal(carb, obs_pred$carbon)
})

test_that("correct values", {
  standardized <- rec |>
    step_range(carbon, hydrogen, min = -12, id = "", clipping = FALSE)

  standardized_trained <- prep(
    standardized,
    training = biomass_tr,
    verbose = FALSE
  )

  obs_pred <- bake(
    standardized_trained,
    new_data = biomass_te,
    all_predictors()
  )
  obs_pred <- as.matrix(obs_pred)

  mins <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, min)
  maxs <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, max)

  new_min <- -12
  new_max <- 1
  new_range <- new_max - new_min

  carb <- ((new_range * (biomass_te$carbon - mins["carbon"])) /
    (maxs["carbon"] - mins["carbon"])) +
    new_min

  hydro <- ((new_range * (biomass_te$hydrogen - mins["hydrogen"])) /
    (maxs["hydrogen"] - mins["hydrogen"])) +
    new_min

  exp_pred <- cbind(carb, hydro)
  colnames(exp_pred) <- c("carbon", "hydrogen")
  expect_equal(exp_pred, obs_pred)
})

test_that("backwards compatibility for before clipping <= 1.0.2 (#1090)", {
  standardized <- rec |>
    step_range(carbon, hydrogen, min = -12, id = "", clipping = TRUE)

  standardized_trained <- prep(
    standardized,
    training = biomass_tr,
    verbose = FALSE
  )

  # simulates old recipe
  standardized_trained$steps[[1]]$clipping <- NULL

  obs_pred <- bake(
    standardized_trained,
    new_data = biomass_te,
    all_predictors()
  )
  obs_pred <- as.matrix(obs_pred)

  mins <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, min)
  maxs <- apply(biomass_tr[, c("carbon", "hydrogen")], 2, max)

  new_min <- -12
  new_max <- 1
  new_range <- new_max - new_min

  carb <- ((new_range * (biomass_te$carbon - mins["carbon"])) /
    (maxs["carbon"] - mins["carbon"])) +
    new_min
  carb <- ifelse(carb > new_max, new_max, carb)
  carb <- ifelse(carb < new_min, new_min, carb)

  hydro <- ((new_range * (biomass_te$hydrogen - mins["hydrogen"])) /
    (maxs["hydrogen"] - mins["hydrogen"])) +
    new_min
  hydro <- ifelse(hydro > new_max, new_max, hydro)
  hydro <- ifelse(hydro < new_min, new_min, hydro)

  exp_pred <- cbind(carb, hydro)
  colnames(exp_pred) <- c("carbon", "hydrogen")
  expect_equal(exp_pred, obs_pred)
})

test_that("warns when NaN is returned due to zero variance", {
  rec <- recipe(~., data = data.frame(x = rep(1, 10))) |>
    step_range(x)
  expect_snapshot(prep(rec))
})

test_that("warns when NaN is returned due to Inf or -Inf", {
  rec <- recipe(~., data = data.frame(x = c(2, 3, 4, Inf))) |>
    step_range(x)
  expect_snapshot(prep(rec))

  rec <- recipe(~., data = data.frame(x = c(2, 3, 4, -Inf))) |>
    step_range(x)
  expect_snapshot(prep(rec))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  standardized <- rec |>
    step_range(carbon, hydrogen, min = -12) |>
    update_role(carbon, hydrogen, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  standardized_trained <- prep(
    standardized,
    training = biomass_tr,
    verbose = FALSE
  )

  expect_snapshot(
    error = TRUE,
    bake(standardized_trained, new_data = biomass_te[, 1:3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_range(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_range(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_range(rec)

  expect <- tibble(
    terms = character(),
    min = double(),
    max = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_range(disp, wt)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_range(disp, wt, max = "max") |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_range(disp, wt, min = "min") |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_range(disp, wt, clipping = "never") |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_range(disp, mpg) |>
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
