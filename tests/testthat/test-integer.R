library(testthat)
library(recipes)

tr_n <- 10
tr_dat <-
  data.frame(
    x = c("a", "d", "c", "a", "c", "a", "a", "d", "c", "c"),
    y = factor(c("B", "B", "D", "B", "C", "D", "A", "D", "C", "C")),
    z = ordered(
      c("Jul", "Apr", "Sep", "Jul", "Nov", "Dec", "Jun", "Feb", "Jan", "Sep"),
      levels = month.abb
    )
  )
tr_dat$x[3] <- NA
tr_dat$y[1] <- NA

te_n <- 4
te_dat <-
  data.frame(
    x = c("d", "c", "c", "a", "?"),
    y = factor(c("E", "D", "C", "C", "??")),
    z = ordered(
      c("Feb", "Aug", "Dec", "Aug", "???"),
      levels = c(month.abb, "???")
    )
  )
te_dat$x[1] <- NA
te_dat$y[1] <- NA

test_that("basic functionality", {
  rec <- recipe(~ x + y + z, data = tr_dat) |>
    step_integer(all_predictors())
  rec_trained <- prep(rec, training = tr_dat)

  tr_int <- bake(rec_trained, new_data = NULL, all_predictors())
  te_int <- bake(rec_trained, te_dat, all_predictors())

  exp_x <- c(NA, 2, 2, 1, 0)
  exp_y <- c(NA, 4, 3, 3, 0)
  exp_z <- c(2, 8, 12, 8, 0)

  expect_equal(te_int$x, exp_x)
  expect_equal(te_int$y, exp_y)
  expect_equal(te_int$z, exp_z)
  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(all(vapply(te_int, is.integer, logical(1))))
})

test_that("zero-based", {
  rec <- recipe(~ x + y + z, data = tr_dat) |>
    step_integer(all_predictors(), zero_based = TRUE)
  rec_trained <- prep(rec, training = tr_dat)

  tr_int <- bake(rec_trained, new_data = NULL, all_predictors())
  te_int <- bake(rec_trained, te_dat, all_predictors())

  exp_x <- c(NA, 1, 1, 0, 3)
  exp_y <- c(NA, 3, 2, 2, 4)
  exp_z <- c(1, 7, 11, 7, 12)

  expect_equal(te_int$x, exp_x)
  expect_equal(te_int$y, exp_y)
  expect_equal(te_int$z, exp_z)
  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(all(vapply(te_int, is.integer, logical(1))))
})

test_that("not integers", {
  rec <- recipe(~ x + y + z, data = tr_dat) |>
    step_integer(all_predictors(), strict = FALSE)
  rec_trained <- prep(rec, training = tr_dat)

  tr_int <- bake(rec_trained, new_data = NULL, all_predictors())
  te_int <- bake(rec_trained, te_dat, all_predictors())

  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(!all(vapply(te_int, is.integer, logical(1))))
  expect_true(all(vapply(tr_int, is.numeric, logical(1))))
  expect_true(!all(vapply(tr_int, is.integer, logical(1))))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~ x + y + z, data = tr_dat) |>
    step_integer(x) |>
    update_role(x, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  rec_trained <- prep(rec, training = tr_dat)

  tr_int <- bake(rec_trained, new_data = NULL, all_predictors())

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, te_dat[, 2:3], all_predictors())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_integer(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_integer(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_integer(rec)

  expect <- tibble(terms = character(), value = list(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~ x + y + z, data = tr_dat) |>
    step_integer(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(~ x + y + z, data = tr_dat) |>
      step_integer(all_predictors(), strict = "yes") |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~ x + y + z, data = tr_dat) |>
      step_integer(all_predictors(), zero_based = "sure!") |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_integer(Species) |>
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
