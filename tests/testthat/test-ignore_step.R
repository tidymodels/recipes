test_that("ignore_step() work correctly", {
  rec <- recipe(mpg ~ ., data = mtcars)

  rec1234 <- recipe(mpg ~ ., data = mtcars) %>%
    step_dummy(all_nominal_predictors(), id = "dummy") %>%
    step_impute_mean(all_numeric_predictors(), id = "impute_mean") %>%
    step_normalize(all_numeric_predictors(), id = "normalize") %>%
    step_pca(all_numeric_predictors(), id = "pca")

  rec234 <- recipe(mpg ~ ., data = mtcars) %>%
    step_impute_mean(all_numeric_predictors(), id = "impute_mean") %>%
    step_normalize(all_numeric_predictors(), id = "normalize") %>%
    step_pca(all_numeric_predictors(), id = "pca")

  rec34 <- recipe(mpg ~ ., data = mtcars) %>%
    step_normalize(all_numeric_predictors(), id = "normalize") %>%
    step_pca(all_numeric_predictors(), id = "pca")

  rec123 <- recipe(mpg ~ ., data = mtcars) %>%
    step_dummy(all_nominal_predictors(), id = "dummy") %>%
    step_impute_mean(all_numeric_predictors(), id = "impute_mean") %>%
    step_normalize(all_numeric_predictors(), id = "normalize")

  expect_equal(
    ignore_attr = TRUE,
    ignore_step(rec1234, number = 1),
    rec234
  )

  expect_equal(
    ignore_attr = TRUE,
    ignore_step(rec1234, number = 1:2),
    rec34
  )

  expect_equal(
    ignore_attr = TRUE,
    ignore_step(rec1234, number = 1:4),
    rec
  )

  expect_equal(
    ignore_attr = TRUE,
    ignore_step(rec1234, number = 1),
    rec234
  )

  expect_equal(
    ignore_attr = TRUE,
    ignore_step(rec1234, id = "pca"),
    rec123
  )
})

test_that("ignore_step() errors when needed", {
  rec <- recipe(mpg ~ ., data = mtcars)

  rec1234 <- recipe(mpg ~ ., data = mtcars) %>%
    step_dummy(all_nominal_predictors(), id = "dummy") %>%
    step_impute_mean(all_numeric_predictors(), id = "impute_mean") %>%
    step_normalize(all_numeric_predictors(), id = "normalize") %>%
    step_pca(all_numeric_predictors(), id = "pca")

  expect_snapshot(
    error = TRUE,
    ignore_step(rec)
  )
  expect_snapshot(
    error = TRUE,
    ignore_step(rec1234)
  )
  expect_snapshot(
    error = TRUE,
    ignore_step(rec1234, number = 1, id = "pca")
  )
  expect_snapshot(
    error = TRUE,
    ignore_step(rec1234, number = 0)
  )
  expect_snapshot(
    error = TRUE,
    ignore_step(rec1234, number = 10)
  )
  expect_snapshot(
    error = TRUE,
    ignore_step(rec1234, id = "no id")
  )
})

test_that("ignore_step() errors when needed", {
  rec12 <- recipe(mpg ~ ., data = mtcars) %>%
    step_dummy(all_nominal_predictors(), id = "dummy") %>%
    step_impute_mean(all_numeric_predictors(), id = "impute_mean") %>%
    prep()
  
  rec1234 <- rec12 %>%
    step_normalize(all_numeric_predictors(), id = "normalize") %>%
    step_pca(all_numeric_predictors(), id = "pca")

  expect_snapshot(
    error = TRUE,
    ignore_step(rec12)
  )
  expect_snapshot(
    error = TRUE,
    ignore_step(rec1234)
  )
})
