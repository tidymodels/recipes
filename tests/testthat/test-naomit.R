library(testthat)
library(recipes)

test_that("step_naomit on all columns", {
  baked <- recipe(~., data = airquality) |>
    step_naomit(all_predictors()) |>
    prep(airquality, verbose = FALSE) |>
    bake(new_data = NULL)

  na_res <- tibble(na.omit(airquality))
  attributes(na_res)$na.action <- NULL

  expect_equal(baked, na_res)
})

test_that("step_naomit on subset of columns", {
  baked <- recipe(Ozone ~ ., data = airquality) |>
    step_naomit(Ozone, Solar.R) |>
    prep(airquality, verbose = FALSE) |>
    bake(new_data = NULL)

  na_res <- tibble(tidyr::drop_na(airquality, Ozone, Solar.R))

  expect_equal(baked, na_res[, c(2:6, 1)])

  baked2 <- recipe(Ozone ~ ., data = airquality) |>
    step_naomit(Solar.R) |>
    prep(airquality, verbose = FALSE) |>
    bake(new_data = NULL)

  na_res2 <- tibble(tidyr::drop_na(airquality, Solar.R))

  expect_equal(baked2, na_res2[, c(2:6, 1)])
})

test_that("doesn't destroy sparsity", {
  mtcars$vs[c(1, 5, 7)] <- NA
  mtcars$vs <- sparsevctrs::as_sparse_double(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_double(mtcars$am)
  rec <- recipe(~ am + vs, data = mtcars) |>
    step_naomit(am, vs)

  rec_trained <- prep(rec, training = mtcars, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = mtcars)

  expect_true(all(vapply(rec_trans, sparsevctrs::is_sparse_double, logical(1))))

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(airquality) |>
    step_naomit(Wind, Temp, skip = FALSE) |>
    update_role(Wind, Temp, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = airquality)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = airquality[, -3]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_naomit(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_naomit(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_naomit(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(Ozone ~ ., data = airquality) |>
    step_naomit(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_naomit(all_numeric_predictors()) |>
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
