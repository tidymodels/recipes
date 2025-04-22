library(testthat)
library(recipes)

n <- 20
set.seed(12)
ex_dat <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n)
)

test_that("simple logit trans", {
  rec <- recipe(~., data = ex_dat) |>
    step_invlogit(x1, id = "")

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_tidy_un <- tibble(terms = "x1", id = "")
  expect_equal(exp_tidy_un, tidy(rec, number = 1))

  exp_tidy_tr <- tibble(terms = "x1", id = "")
  expect_equal(exp_tidy_tr, tidy(rec_trained, number = 1))

  exp_res <- as_tibble(ex_dat)
  exp_res$x1 <- binomial()$linkinv(exp_res$x1)
  expect_equal(rec_trans, exp_res)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = ex_dat) |>
    step_invlogit(x1) |>
    update_role(x1, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = ex_dat[, 2, drop = FALSE])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_invlogit(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_invlogit(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_invlogit(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_invlogit(x1)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_invlogit(mpg, disp) |>
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
