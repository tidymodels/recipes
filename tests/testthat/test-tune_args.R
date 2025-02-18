test_that("tune_args() errors on multiple tune()s in same arg", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_pca(all_predictors(), num_comp = ~tune() + tune()) %>%
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
