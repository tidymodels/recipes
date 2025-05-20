test_that("tune_args() errors on multiple tune()s in same arg", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_pca(all_predictors(), num_comp = ~ tune() + tune()) |>
      tune_args()
  )
})

test_that("tune_tbl() errors on duplicate ids", {
  expect_snapshot(
    error = TRUE,
    tune_tbl(
      name = c("a", "b"),
      tunable = c(TRUE, TRUE),
      id = c("a", "a"),
      source = c("a", "b"),
      component = c("a", "b"),
      component_id = c("a", "b"),
      full = TRUE
    )
  )
})

test_that("tune_args() returns all arguments marked for tuning (#1296)", {
  rec <- recipe(~., data = mtcars) |>
    step_impute_bag(
      all_predictors(),
      # tunable
      trees = hardhat::tune(),
      # not known tunable
      options = hardhat::tune(),
      id = ""
    )

  exp <- tibble::tibble(
    name = c("trees", "options"),
    tunable = c(TRUE, FALSE),
    id = c("trees", "options"),
    source = "recipe",
    component = "step_impute_bag",
    component_id = "",
  )

  expect_identical(
    tune_args(rec),
    exp
  )
})

test_that("tune_args() doesn't error on namespaced selectors", {
  exp <- tibble::tibble(
    name = character(0),
    tunable = logical(0),
    id = character(0),
    source = character(0),
    component = character(0),
    component_id = character(0),
  )

  expect_identical(
    recipe(~., data = mtcars) |>
      step_pca(dplyr::contains("a")) |>
      tune_args(),
    exp
  )
})

test_that("tune_args() wont detect tune() calls in parsnip objects (#1506)", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rpart")

  base_model <- parsnip::decision_tree(cost_complexity = hardhat::tune())

  rec_empty <- recipe(mpg ~ ., data = mtcars) |>
    step_testthat_helper(output = base_model)

  rec_mod <- recipe(mpg ~ ., data = mtcars) |>
    step_testthat_helper()

  expect_identical(
    extract_parameter_set_dials(rec_empty),
    extract_parameter_set_dials(rec_mod)
  )
})
