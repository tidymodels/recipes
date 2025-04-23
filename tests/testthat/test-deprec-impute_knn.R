test_that("Deprecation error", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_knnimpute()
  )
})
