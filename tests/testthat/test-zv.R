library(testthat)
library(recipes)

n <- 50
set.seed(424)
dat <- data.frame(
  x1 = rnorm(n),
  x2 = rep(1:5, each = 10),
  x3 = factor(rep(letters[1:3], c(2, 2, 46))),
  x4 = 1,
  y = runif(n)
)

ratios <- function(x) {
  tab <- sort(table(x), decreasing = TRUE)
  if (length(tab) > 1) {
    tab[1] / tab[2]
  } else {
    Inf
  }
}

pct_uni <- vapply(dat[, -5], function(x) length(unique(x)), c(val = 0)) /
  nrow(dat) *
  100
f_ratio <- vapply(dat[, -5], ratios, c(val = 0))
vars <- names(pct_uni)

test_that("zv filtering", {
  rec <- recipe(y ~ ., data = dat)
  filtering <- rec |>
    step_zv(x1, x2, x3, x4)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  expect_equal(filtering_trained$steps[[1]]$removals, "x4")
})

test_that("group-wise zv filtering", {
  mtcars0 <- mtcars |>
    mutate(
      const = 0,
      group1 = am,
      group2 = vs
    )

  rec_group1 <- recipe(~., data = mtcars0) |>
    step_zv(all_predictors(), group = "group1") |>
    prep()

  expect_equal(rec_group1$steps[[1]]$removals, c("am", "const"))

  rec_group2 <- recipe(~., data = mtcars0) |>
    step_zv(all_predictors(), group = "group2") |>
    prep()

  expect_equal(rec_group2$steps[[1]]$removals, c("vs", "const"))

  rec_group12 <- recipe(~., data = mtcars0) |>
    step_zv(all_predictors(), group = c("group1", "group2")) |>
    prep()

  expect_equal(
    rec_group12$steps[[1]]$removals,
    c("cyl", "vs", "am", "gear", "const")
  )

  rec_group12_vars <- recipe(~., data = mtcars0) |>
    step_zv(all_predictors(), group = vars(group1, group2)) |>
    prep()

  expect_equal(
    rec_group12_vars$steps[[1]]$removals,
    c("cyl", "vs", "am", "gear", "const")
  )
})

test_that("mssing values in zero-variance screen", {
  x <- rep(1, 5)
  y <- c(NA, x)
  z <- rep(NA, 5)

  expect_true(recipes:::one_unique(x))
  expect_true(recipes:::one_unique(y))
  expect_true(recipes:::one_unique(z))
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_integer(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)
  rec <- recipe(~ am + vs, data = mtcars) |>
    step_zv(all_predictors())

  rec_trained <- prep(rec, training = mtcars, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = mtcars)

  expect_true(
    all(vapply(rec_trans, sparsevctrs::is_sparse_integer, logical(1)))
  )

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_zv() removes variables and thus does not care if they are not there.
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_zv(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_zv(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_zv(rec)

  expect <- tibble(
    terms = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_zv(rec)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_zv(all_predictors()) |>
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
