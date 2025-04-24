library(testthat)

test_that('correct means', {
  exp_means <- colMeans(mtcars)
  calc_means <- averages(mtcars)
  expect_equal(exp_means, calc_means)

  exp_means <- cov.wt(mtcars, frac_wts)$center
  calc_means <- averages(mtcars, frac_wts)
  expect_equal(exp_means, calc_means)

  exp_means <- cov.wt(mtcars, freq_wts)$center
  calc_means <- averages(mtcars, freq_wts)
  expect_equal(exp_means, calc_means)

  # Missing case weight
  exp_means <- cov.wt(mtcars[-1, ], freq_wts[-1])$center
  calc_means <- averages(mtcars, miss_wts)
  expect_equal(exp_means, calc_means)

  # Missing data in x
  exp_means <- cov.wt(mtcars, freq_wts)$center
  exp_means[2] <- weighted.mean(mtcar_mis[[2]], freq_wts, na.rm = TRUE)
  exp_means[3] <- weighted.mean(mtcar_mis[[3]], freq_wts, na.rm = TRUE)
  calc_means <- averages(mtcar_mis, freq_wts)
  expect_equal(exp_means, calc_means)
})

test_that('correct medians', {
  exp_medians <- apply(mtcars, 2, median)
  calc_medians <- medians(mtcars)
  expect_equal(exp_medians, calc_medians)

  exp_medians <- map_dbl(mtcars, weighted_median_impl, wts = frac_wts)
  calc_medians <- medians(mtcars, frac_wts)
  expect_equal(exp_medians, calc_medians)

  exp_medians <- map_dbl(mtcars, weighted_median_impl, wts = freq_wts)
  calc_medians <- medians(mtcars, freq_wts)
  expect_equal(exp_medians, calc_medians)

  # Missing case weight
  exp_medians <- map_dbl(mtcars[-1, ], weighted_median_impl, wts = freq_wts[-1])
  calc_medians <- medians(mtcars, miss_wts)
  expect_equal(exp_medians, calc_medians)

  # Missing data in x
  exp_medians <- map_dbl(mtcars, weighted_median_impl, wts = freq_wts)
  exp_medians[2] <- weighted_median_impl(mtcar_mis[[2]][-1], freq_wts[-1])
  exp_medians[3] <- weighted_median_impl(mtcar_mis[[3]][-2], freq_wts[-2])
  calc_medians <- medians(mtcar_mis, freq_wts)
  expect_equal(exp_medians, calc_medians)
})

test_that('correct variances', {
  exp_vars <- diag(var(mtcars))
  calc_vars <- variances(mtcars)
  expect_equal(exp_vars, calc_vars)

  exp_vars <- diag(cov.wt(mtcars, frac_wts)$cov)
  calc_vars <- variances(mtcars, frac_wts)
  expect_equal(exp_vars, calc_vars)

  exp_vars <- diag(cov.wt(mtcars, freq_wts)$cov)
  calc_vars <- variances(mtcars, freq_wts)
  expect_equal(exp_vars, calc_vars)

  # Missing case weight
  exp_vars <- diag(cov.wt(mtcars[-1, ], freq_wts[-1])$cov)
  calc_vars <- variances(mtcars, miss_wts)
  expect_equal(exp_vars, calc_vars)

  # Missing data in x
  exp_vars <- diag(cov.wt(mtcars, freq_wts)$cov)
  exp_vars[2] <- diag(cov.wt(mtcar_mis[-1, 2, drop = FALSE], freq_wts[-1])$cov)
  exp_vars[3] <- diag(cov.wt(mtcar_mis[-2, 3, drop = FALSE], freq_wts[-2])$cov)
  calc_vars <- variances(mtcar_mis, freq_wts)
  expect_equal(exp_vars, calc_vars)

  # Missing data in x with na_rm = FALSE
  exp_vars <- diag(cov.wt(mtcars, freq_wts)$cov)
  exp_vars[map_lgl(mtcar_mis, anyNA)] <- NA

  calc_vars <- variances(mtcar_mis, freq_wts, na_rm = FALSE)
  expect_equal(exp_vars, calc_vars)
})

test_that('correct covariances', {
  exp_cors <- cov(mtcars)
  calc_cors <- covariances(mtcars)
  expect_equal(exp_cors, calc_cors)

  exp_cors <- cov.wt(mtcars, frac_wts, cor = FALSE)$cov
  calc_cors <- covariances(mtcars, frac_wts)
  expect_equal(exp_cors, calc_cors)

  exp_cors <- cov.wt(mtcars, freq_wts, cor = FALSE)$cov
  calc_cors <- covariances(mtcars, freq_wts)
  expect_equal(exp_cors, calc_cors)

  # Missing case weight
  exp_cors <- cov.wt(mtcars[-1, ], freq_wts[-1], cor = FALSE)$cov
  calc_cors <- covariances(mtcars, miss_wts)
  expect_equal(exp_cors, calc_cors)

  # Missing data in x
  exp_cors <- cov.wt(mtcars[-(1:2), 1:3], freq_wts[-(1:2)], cor = FALSE)$cov
  calc_cors <- covariances(mtcar_mis[, 1:3], freq_wts)
  expect_equal(exp_cors, calc_cors)
})

test_that('correct correlations', {
  exp_cors <- cor(mtcars)
  calc_cors <- correlations(mtcars)
  expect_equal(exp_cors, calc_cors)

  exp_cors <- cov.wt(mtcars, frac_wts, cor = TRUE)$cor
  calc_cors <- correlations(mtcars, frac_wts)
  expect_equal(exp_cors, calc_cors)

  exp_cors <- cov.wt(mtcars, freq_wts, cor = TRUE)$cor
  calc_cors <- correlations(mtcars, freq_wts)
  expect_equal(exp_cors, calc_cors)

  # Missing case weight
  exp_cors <- cov.wt(mtcars[-1, ], freq_wts[-1], cor = TRUE)$cor
  calc_cors <- correlations(mtcars, miss_wts)
  expect_equal(exp_cors, calc_cors)

  # Missing data in x
  exp_cors <- cov.wt(mtcars[-(1:2), 1:3], freq_wts[-(1:2)], cor = TRUE)$cor
  calc_cors <- correlations(mtcar_mis[, 1:3], freq_wts)
  expect_equal(exp_cors, calc_cors)
})

test_that("is_unsupervised_weights works", {
  expect_true(is_unsupervised_weights(frequency_weights(5)))
  expect_false(is_unsupervised_weights(importance_weights(5)))

  expect_snapshot(error = TRUE, is_unsupervised_weights(3))
})

test_that("are_weights_used works", {
  expect_null(are_weights_used(NULL))
  expect_true(are_weights_used(frequency_weights(1)))
  expect_true(are_weights_used(importance_weights(1)))

  expect_null(are_weights_used(NULL, unsupervised = TRUE))
  expect_false(are_weights_used(importance_weights(1), unsupervised = TRUE))
  expect_true(are_weights_used(frequency_weights(1), unsupervised = TRUE))
})

test_that("is_unsupervised_weights works", {
  expect_snapshot(
    error = TRUE,
    too_many_case_weights(c("var1", "var2"))
  )
})

test_that("get_case_weights() catches non-numeric case weights", {
  mtcars$vs <- as.character(mtcars$vs)
  class(mtcars$vs) <- c("hardhat_case_weights", "non_numeric_weights")

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_normalize(all_predictors()) |>
      prep()
  )
})
