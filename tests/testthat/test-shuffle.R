library(testthat)
library(recipes)

n <- 50
set.seed(424)
dat <- data.frame(
  x1 = sort(rnorm(n)),
  x2 = sort(rep(1:5, each = 10)),
  x3 = sort(factor(rep(letters[1:3], c(2, 2, 46)))),
  x4 = 1,
  y = sort(runif(n))
)

test_that("numeric data", {
  rec1 <- recipe(y ~ ., data = dat) |>
    step_shuffle(all_numeric())

  rec1 <- prep(rec1, training = dat, verbose = FALSE)
  set.seed(7046)
  dat1 <- bake(rec1, dat, all_predictors())
  exp1 <- c(FALSE, FALSE, TRUE, TRUE)
  obs1 <- rep(NA, 4)
  for (i in 1:ncol(dat1)) {
    obs1[i] <-
      isTRUE(all.equal(dat[, i], getElement(dat1, names(dat)[i])))
  }
  expect_equal(exp1, obs1)
})

test_that("nominal data", {
  rec2 <- recipe(y ~ ., data = dat) |>
    step_shuffle(all_nominal())

  rec2 <- prep(rec2, training = dat, verbose = FALSE)
  set.seed(804)
  dat2 <- bake(rec2, dat, all_predictors())
  exp2 <- c(TRUE, TRUE, FALSE, TRUE)
  obs2 <- rep(NA, 4)
  for (i in 1:ncol(dat2)) {
    obs2[i] <-
      isTRUE(all.equal(dat[, i], getElement(dat2, names(dat)[i])))
  }
  expect_equal(exp2, obs2)
})

test_that("all data", {
  rec3 <- recipe(y ~ ., data = dat) |>
    step_shuffle(all_predictors())

  rec3 <- prep(rec3, training = dat, verbose = FALSE)
  set.seed(2516)
  dat3 <- bake(rec3, dat, all_predictors())
  exp3 <- c(FALSE, FALSE, FALSE, TRUE)
  obs3 <- rep(NA, 4)
  for (i in 1:ncol(dat3)) {
    obs3[i] <-
      isTRUE(all.equal(dat[, i], getElement(dat3, names(dat)[i])))
  }
  expect_equal(exp3, obs3)
})

test_that("bake a single row", {
  rec4 <- recipe(y ~ ., data = dat) |>
    step_shuffle(all_predictors())

  rec4 <- prep(rec4, training = dat, verbose = FALSE)
  expect_snapshot(dat4 <- bake(rec4, dat[1, ]))
  expect_equal(dat4, tibble(dat[1, ]))
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)

  rec <- recipe(~., mtcars) |>
    step_shuffle(vs, am) |>
    prep()

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$vs))
  expect_true(sparsevctrs::is_sparse_integer(bake(rec, NULL)$am))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec1 <- recipe(y ~ ., data = dat) |>
    step_shuffle(all_numeric()) |>
    update_role(all_numeric(), new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec1 <- prep(rec1, training = dat, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(rec1, dat[, 2:5]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_shuffle(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_shuffle(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_shuffle(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(y ~ ., data = dat) |>
    step_shuffle(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_shuffle(all_predictors()) |>
    prep()

  suppressWarnings(
    expect_identical(
      nrow(bake(rec, slice(data, 1))),
      1L
    )
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
