test_that("step_nnmf() is deprecated", {
  expect_snapshot(
    suppressMessages(
      recipe(~., data = mtcars) |> step_nnmf(all_numeric_predictors())
    )
  )
})
