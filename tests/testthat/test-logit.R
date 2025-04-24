library(testthat)
library(recipes)

n <- 20
set.seed(12)
ex_dat <- data.frame(
  x1 = runif(n),
  x2 = rnorm(n),
  x3 = seq(0, 1, length.out = 20)
)

test_that("simple logit trans", {
  rec <- recipe(~., data = ex_dat) |>
    step_logit(x1)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  exp_res <- as_tibble(ex_dat)
  exp_res$x1 <- binomial()$linkfun(exp_res$x1)
  expect_equal(rec_trans, exp_res)

  rec <- recipe(~., data = ex_dat) |>
    step_logit(x3, offset = 0.1)
  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)
  exp_res <-
    as_tibble(ex_dat) |>
    mutate(
      x3 = case_when(
        x3 == 1.0 ~ 1 - 0.1,
        x3 == 0.0 ~ 0.1,
        TRUE ~ x3
      )
    )
  exp_res$x3 <- binomial()$linkfun(exp_res$x3)
  expect_equal(rec_trans, exp_res)
})

test_that("out of bounds logit trans", {
  rec <- recipe(~., data = ex_dat) |>
    step_logit(x1, x2)

  expect_snapshot(error = TRUE, prep(rec, training = ex_dat, verbose = FALSE))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = ex_dat) |>
    step_logit(x1) |>
    update_role(x1, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = ex_dat[, 2:3]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_logit(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_logit(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_logit(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_logit(x1)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = ex_dat) |>
      step_logit(x1, offset = "sure") |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_logit(vs, am) |>
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
