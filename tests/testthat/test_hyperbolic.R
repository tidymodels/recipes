library(testthat)
library(recipes)
library(tibble)


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
    rec <- recipe(~., data = ex_dat) %>%
      step_hyperbolic(x1, x2, func = func, inverse = FALSE)

    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    rec_trans <- bake(rec_trained, new_data = ex_dat)

    foo <- get(func)
    exp_res <- get_exp(ex_dat, foo)
    expect_equal(rec_trans, exp_res)
  }

  for (func in c("sinh", "tanh")) {
    rec <- recipe(~., data = ex_dat) %>%
      step_hyperbolic(x1, x2, func = func, inverse = TRUE)

    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    rec_trans <- bake(rec_trained, new_data = ex_dat)

    foo <- get(paste0("a", func))
    exp_res <- get_exp(ex_dat, foo)
    expect_equal(rec_trans, exp_res)
  }

  rec <- recipe(~., data = ex_dat1) %>%
    step_hyperbolic(x1, x2, func = "cosh", inverse = TRUE)
  rec_trained <- prep(rec, training = ex_dat1, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat1)
  exp_res <- get_exp(ex_dat1, "acosh")
  expect_equal(rec_trans, exp_res)

})


test_that("printing", {
  rec <- recipe(~., data = ex_dat) %>%
    step_hyperbolic(x1, x2, func = "sinh", inverse = TRUE)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
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

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_hyperbolic(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("wrong function", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  expect_snapshot_error(step_hyperbolic(rec, func = "cos"))
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = ex_dat) %>%
    step_hyperbolic(x1, x2, func = "sinh", inverse = FALSE) %>%
    update_role(x1, x2, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_error(bake(rec_trained, new_data = ex_dat[, 2, drop = FALSE]),
               class = "new_data_missing_column")
})
