library(recipes)
library(testthat)

n <- 200

set.seed(8575)
ex_dat <- data.frame(
  w = sample(letters[1:3], size = n, replace = TRUE),
  x = sample(LETTERS[1:2], size = n, replace = TRUE),
  y = factor(rep_len(month.abb, n)),
  z = factor(rep_len(month.name, n), ordered = TRUE),
  stringsAsFactors = FALSE
)

test_that("basic functionality", {
  ex_1 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_factor2string(y, z) |>
    prep(ex_dat) |>
    bake(new_data = NULL)
  expect_equal(class(ex_1$w), "character")
  expect_equal(class(ex_1$x), "character")
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
      step_factor2string(w, x) |>
      prep(ex_dat)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  ex_1 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_factor2string(y, z) |>
    update_role(y, z, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep(ex_dat)

  expect_snapshot(error = TRUE, bake(ex_1, new_data = ex_dat[, 1:3]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_factor2string(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_factor2string(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_factor2string(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_factor2string(y, z)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_factor2string(all_nominal_predictors()) |>
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
