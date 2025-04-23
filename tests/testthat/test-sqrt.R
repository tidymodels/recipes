library(testthat)
library(recipes)

n <- 20
ex_dat <- data.frame(
  x1 = seq(0, 1, length = n),
  x2 = rep(1:5, 4)
)

test_that("simple sqrt trans", {
  rec <- recipe(~., data = ex_dat) |>
    step_sqrt(x1, x2)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat, sqrt))
  expect_equal(rec_trans, exp_res)
})

test_that("doesn't destroy sparsity", {
  ex_dat$x1 <- sparsevctrs::as_sparse_double(ex_dat$x1)
  ex_dat$x2 <- sparsevctrs::as_sparse_integer(ex_dat$x2)
  rec <- recipe(~., data = ex_dat) |>
    step_sqrt(x1, x2)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  expect_true(all(vapply(rec_trans, sparsevctrs::is_sparse_double, logical(1))))

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = ex_dat) |>
    step_sqrt(x1, x2) |>
    update_role(x1, x2, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep(ex_dat)

  expect_snapshot(error = TRUE, bake(rec, new_data = ex_dat[, 2, drop = FALSE]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_sqrt(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_sqrt(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_sqrt(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_sqrt(x1, x2)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_sqrt(mpg, disp) |>
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
