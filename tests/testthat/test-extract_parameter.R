test_that("rethrows error correctly from implementation", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_pca(all_predictors(), num_comp = hardhat::tune())

  local_mocked_bindings(
    .package = "dials",
    num_comp = function(...) {
      cli::cli_abort("mocked error")
    }
  )

  expect_snapshot(
    error = TRUE,
    params <- extract_parameter_set_dials(rec)
  )
})
