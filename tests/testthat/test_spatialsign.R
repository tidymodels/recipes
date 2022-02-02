library(testthat)
library(recipes)
library(modeldata)
data(biomass)

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('spatial sign', {
  sp_sign <- rec %>%
    step_center(carbon, hydrogen) %>%
    step_scale(carbon, hydrogen) %>%
    step_spatialsign(carbon, hydrogen)

  sp_sign_trained <- prep(sp_sign, training = biomass, verbose = FALSE)

  sp_sign_pred <- bake(sp_sign_trained, new_data = biomass)
  sp_sign_pred <- as.matrix(sp_sign_pred)[, c("carbon", "hydrogen")]

  x <- as.matrix(scale(biomass[, 3:4], center = TRUE, scale = TRUE))
  x <- t(apply(x, 1, function(x) x/sqrt(sum(x^2))))

  expect_equal(sp_sign_pred, x)
})

test_that('Missing values', {
  sp_sign <- rec %>%
    step_spatialsign(carbon, hydrogen)

  sp_sign_trained <- prep(sp_sign, training = biomass, verbose = FALSE)

  with_na <- head(biomass)
  with_na$carbon[1] <- NA
  with_na$hydrogen[2] <- NA
  rownames(with_na) <- NULL

  sp_sign_pred <- bake(sp_sign_trained, new_data = with_na)
  sp_sign_pred <- as.matrix(sp_sign_pred)[, c("carbon", "hydrogen")]

  x <- as.matrix(with_na[, 3:4])
  x <- t(apply(x, 1, function(x) x/sqrt(sum(x^2, na.rm = TRUE))))

  expect_equal(sp_sign_pred, x)
})


test_that('printing', {
  sp_sign <- rec %>%
    step_center(carbon, hydrogen) %>%
    step_scale(carbon, hydrogen) %>%
    step_spatialsign(carbon, hydrogen)
  expect_output(print(sp_sign))
  expect_output(prep(sp_sign, training = biomass, verbose = TRUE))
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

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_spatialsign(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
