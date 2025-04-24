library(testthat)
library(recipes)

n <- 20
set.seed(1)
ex_dat <- data.frame(
  x1 = runif(n, min = -1, max = 1),
  x2 = runif(n, min = -1, max = 1)
)

set.seed(2)
ex_dat1 <- data.frame(
  x1 = runif(n, min = 1, max = 5),
  x2 = runif(n, min = 1, max = 5)
)

get_exp <- function(x, f) {
  as_tibble(lapply(x, f))
}

test_that("simple hyperbolic trans", {
  for (func in c("sinh", "cosh", "tanh")) {
    rec <- recipe(~., data = ex_dat) |>
      step_hyperbolic(x1, x2, func = func, inverse = FALSE)

    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    rec_trans <- bake(rec_trained, new_data = ex_dat)

    foo <- get(func)
    exp_res <- get_exp(ex_dat, foo)
    expect_equal(rec_trans, exp_res)
  }

  for (func in c("sinh", "tanh")) {
    rec <- recipe(~., data = ex_dat) |>
      step_hyperbolic(x1, x2, func = func, inverse = TRUE)

    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    rec_trans <- bake(rec_trained, new_data = ex_dat)

    foo <- get(paste0("a", func))
    exp_res <- get_exp(ex_dat, foo)
    expect_equal(rec_trans, exp_res)
  }

  rec <- recipe(~., data = ex_dat1) |>
    step_hyperbolic(x1, x2, func = "cosh", inverse = TRUE)
  rec_trained <- prep(rec, training = ex_dat1, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat1)
  exp_res <- get_exp(ex_dat1, "acosh")
  expect_equal(rec_trans, exp_res)
})

test_that("wrong arguments", {
  rec <- recipe(mpg ~ ., mtcars)
  expect_snapshot(step_hyperbolic(rec, func = "cos") |> prep(), error = TRUE)
  expect_snapshot(step_hyperbolic(rec, inverse = 2) |> prep(), error = TRUE)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = ex_dat) |>
    step_hyperbolic(x1, x2, func = "sinh", inverse = FALSE) |>
    update_role(x1, x2, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = ex_dat[, 2, drop = FALSE])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_hyperbolic(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_hyperbolic(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_hyperbolic(rec)

  expect <- tibble(
    terms = character(),
    inverse = logical(),
    func = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_hyperbolic(x1, x2)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_hyperbolic(disp) |>
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
