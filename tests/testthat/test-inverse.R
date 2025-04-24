library(testthat)
library(recipes)

n <- 20
set.seed(1)
ex_dat <- data.frame(
  x1 = exp(rnorm(n, mean = .1)),
  x2 = 1 / abs(rnorm(n)),
  x3 = rep(1:2, each = n / 2),
  x4 = rexp(n),
  x5 = rep(0:1, each = n / 2)
)

test_that("simple inverse trans", {
  rec <- recipe(~ x1 + x2 + x3 + x4, data = ex_dat) |>
    step_inverse(x1, x2, x3, x4)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat[, -5], function(x) 1 / x))

  expect_equal(rec_trans, exp_res)
})

test_that("alt offset", {
  rec <- recipe(~., data = ex_dat) |>
    step_inverse(x1, x2, x3, x4, x5, offset = 0.1)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(lapply(ex_dat, function(x) 1 / (x + 0.1)))

  expect_equal(rec_trans, exp_res)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~ x1 + x2 + x3 + x4, data = ex_dat) |>
    step_inverse(x1, x2, x3, x4) |>
    update_role(x1, x2, x3, x4, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = ex_dat[, 1:3]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_inverse(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_inverse(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_inverse(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_inverse(x1, x2, x3, x4)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = ex_dat) |>
      step_inverse(x1, x2, x3, x4, offset = function(x) x / 3) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_inverse(mpg, disp) |>
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
