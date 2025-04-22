library(testthat)

x <- -10:110

test_that("core function - correct input", {
  expect_no_error(range_check_func(x, -10, 110))
  expect_snapshot(error = TRUE, range_check_func(as.character(x), -10, 110))
  expect_snapshot(error = TRUE, range_check_func(x, -10, 110, "a"))
  expect_no_error(range_check_func(x, -10, 110, .05))
  expect_no_error(range_check_func(x, -10, 110, c(.05, .08)))
  expect_snapshot(error = TRUE, range_check_func(x, -10, 110, c(.05, .08, .05)))
})

test_that("core function - workings", {
  expect_no_error(range_check_func(x, -5, 110))
  expect_snapshot(error = TRUE, range_check_func(x, 0, 100))
  expect_snapshot(error = TRUE, range_check_func(x, 0, 110))
  expect_snapshot(error = TRUE, range_check_func(x, -5, 100))
  expect_snapshot(
    error = TRUE,
    range_check_func(x, 0, 100, slack_prop = c(0.05, 0.1))
  )
  expect_snapshot(
    error = TRUE,
    range_check_func(x, 0, 100, slack_prop = c(0.1, 0.05))
  )
  expect_snapshot(
    range_check_func(x, 0, 100, warn = TRUE)
  )
})

test_that("in recipe", {
  train <- tibble(x = c(0, 100), y = c(0, 50))
  test <- tibble(x = c(-10, 110), y = c(-10, 60))
  rec1 <- recipe(train) |>
    check_range(x, y, slack_prop = 0.2) |>
    prep()
  expect_no_warning(bake(rec1, test))
  expect_no_error(bake(rec1, test))

  rec2 <- recipe(train) |>
    check_range(x, y) |>
    prep()
  expect_snapshot(error = TRUE, bake(rec2, test))

  rec3 <- recipe(train) |>
    check_range(x, y, warn = TRUE) |>
    prep()
  expect_snapshot(bake(rec3, test))

  rec4 <- recipe(train) |>
    check_range(y, slack_prop = c(0.2, 0.1)) |>
    prep()
  expect_snapshot(error = TRUE, bake(rec4, test))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(mtcars) |>
    check_range(disp) |>
    update_role(disp, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = mtcars)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = mtcars[, -3]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_range(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_range(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_range(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(mtcars) |>
    check_range(drat, cyl, am)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    check_range(disp, mpg) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
