test_that("extract_fit_time() works", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_scale(all_numeric_predictors(), id = "scale") |>
    step_center(all_numeric_predictors(), id = "center") |>
    prep()

  res <- extract_fit_time(rec)

  expect_true(is_tibble(res))
  expect_identical(names(res), c("stage_id", "elapsed"))
  expect_identical(res$stage_id, "recipe")
  expect_true(is.double(res$elapsed))
  expect_true(res$elapsed >= 0)

  res <- extract_fit_time(rec, summarize = FALSE)

  expect_true(is_tibble(res))
  expect_identical(names(res), c("stage_id", "elapsed"))
  expect_identical(
    res$stage_id,
    c("prep.scale", "bake.scale", "prep.center", "bake.center")
  )
  expect_true(is.double(res$elapsed))
  expect_true(all(res$elapsed >= 0))

  rec$fit_times <- NULL

  expect_snapshot(
    error = TRUE,
    extract_fit_time(rec)
  )
})

test_that("extract_fit_time() works with no steps", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    prep()

  res <- extract_fit_time(rec)

  expect_true(is_tibble(res))
  expect_identical(names(res), c("stage_id", "elapsed"))
  expect_identical(res$stage_id, "recipe")
  expect_true(is.double(res$elapsed))
  expect_true(res$elapsed >= 0)

  res <- extract_fit_time(rec, summarize = FALSE)

  expect_true(is_tibble(res))
  expect_identical(names(res), c("stage_id", "elapsed"))
  expect_identical(res$stage_id, character())
  expect_true(is.double(res$elapsed))
  expect_true(all(res$elapsed >= 0))
})
