test_that("check_new_data works", {
  set.seed(313)
  examples <- matrix(exp(rnorm(40)), ncol = 4)
  examples <- as.data.frame(examples)

  rec <- recipe(~ V1 + V2 + V3 + V4, data = examples)

  log_trans <- rec %>%
    step_log(all_numeric_predictors())

  log_obj <- prep(log_trans, training = examples)

  expect_snapshot(bake(log_obj, examples[,1:3, drop = FALSE]), error = TRUE)
  expect_snapshot(bake(log_obj, examples[,1:2, drop = FALSE]), error = TRUE)
  expect_snapshot(bake(log_obj, examples[,1  , drop = FALSE]), error = TRUE)
})
