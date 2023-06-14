test_that("check_new_data works", {
  set.seed(313)
  examples <- matrix(exp(rnorm(40)), ncol = 4)
  examples <- as.data.frame(examples)

  log_trans <- recipe(~ V1 + V2 + V3 + V4, data = examples) %>%
    step_log(V1, V2, V3) %>%
    update_role(V1, V2, V3, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  log_obj <- prep(log_trans, training = examples)

  expect_snapshot(bake(log_obj, examples[,2:4, drop = FALSE]), error = TRUE)
  expect_snapshot(bake(log_obj, examples[,3:4, drop = FALSE]), error = TRUE)
  expect_snapshot(bake(log_obj, examples[,  4, drop = FALSE]), error = TRUE)
})

test_that("conditionMessage method for recipes errors works", {
  res <-
    try({
      recipe(~ ., data = mtcars) %>%
        step_dummy(all_numeric_predictors()) %>%
        prep()},
      silent = TRUE
    )

  expect_s3_class(attr(res, "condition"), "recipes_error")

  expect_snapshot(conditionMessage(attr(res, "condition")))
})
