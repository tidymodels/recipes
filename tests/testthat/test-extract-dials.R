test_that("extract parameter set from recipe with no steps", {
  skip_if_not_installed("dials")

  bare_rec <- recipe(mpg ~ ., data = mtcars)

  bare_info <- extract_parameter_set_dials(bare_rec)
  check_parameter_set_tibble(bare_info)
  expect_equal(nrow(bare_info), 0)
})

test_that("extract parameter set from recipe with no tunable parameters", {
  skip_if_not_installed("dials")

  rm_rec <-
    recipe(mpg ~ ., data = mtcars) |>
    step_rm(hp)

  rm_info <- extract_parameter_set_dials(rm_rec)
  check_parameter_set_tibble(rm_info)
  expect_equal(nrow(rm_info), 0)
})

test_that("extract parameter set from recipe with tunable parameters", {
  skip_if_not_installed("dials")

  spline_rec <-
    recipe(mpg ~ ., data = mtcars) |>
    step_impute_knn(
      all_numeric_predictors(),
      neighbors = hardhat::tune("imputation")
    ) |>
    step_other(all_nominal(), threshold = hardhat::tune()) |>
    step_dummy(all_nominal()) |>
    step_normalize(all_predictors()) |>
    step_bs(
      all_predictors(),
      deg_free = hardhat::tune(),
      degree = hardhat::tune()
    )

  spline_info <- extract_parameter_set_dials(spline_rec)
  check_parameter_set_tibble(spline_info)
  expected_cols <- c("step_impute_knn", "step_other", "step_bs", "step_bs")
  expect_equal(
    spline_info$component,
    expected_cols
  )
  expect_true(all(spline_info$source == "recipe"))
  nms <- c("neighbors", "threshold", "deg_free", "degree")
  expect_equal(spline_info$name, nms)
  ids <- c("imputation", "threshold", "deg_free", "degree")
  expect_equal(spline_info$id, ids)

  expect_equal(spline_info$object[[1]], dials::neighbors(c(1, 10)))
  expect_equal(spline_info$object[[2]], dials::threshold(c(0, 1 / 10)))
  expect_equal(spline_info$object[[3]], dials::spline_degree(c(1, 15)))
  expect_equal(spline_info$object[[4]], dials::degree_int(c(1, 2)))
})

# -------------------------------------------------------------------------

test_that("extract single parameter from recipe with no steps", {
  skip_if_not_installed("dials")

  bare_rec <- recipe(mpg ~ ., data = mtcars)

  expect_snapshot(
    error = TRUE,
    extract_parameter_dials(bare_rec, parameter = "none there")
  )
})

test_that("extract single parameter from recipe with no tunable parameters", {
  skip_if_not_installed("dials")

  rm_rec <-
    recipe(mpg ~ ., data = mtcars) |>
    step_rm(hp)

  expect_snapshot(
    error = TRUE,
    extract_parameter_dials(rm_rec, parameter = "none there")
  )
})

test_that("extract single parameter from recipe with tunable parameters", {
  skip_if_not_installed("dials")

  spline_rec <-
    recipe(mpg ~ ., data = mtcars) |>
    step_impute_knn(
      all_numeric_predictors(),
      neighbors = hardhat::tune("imputation")
    ) |>
    step_other(all_nominal(), threshold = hardhat::tune()) |>
    step_dummy(all_nominal()) |>
    step_normalize(all_predictors()) |>
    step_bs(
      all_predictors(),
      deg_free = hardhat::tune(),
      degree = hardhat::tune()
    )

  expect_equal(
    extract_parameter_dials(spline_rec, "imputation"),
    dials::neighbors()
  )
  expect_equal(
    extract_parameter_dials(spline_rec, "threshold"),
    dials::threshold(c(0, 1 / 10))
  )
  expect_equal(
    extract_parameter_dials(spline_rec, "deg_free"),
    dials::spline_degree(range = c(1, 15))
  )
  expect_equal(
    extract_parameter_dials(spline_rec, "degree"),
    dials::degree_int(c(1, 2))
  )
})
