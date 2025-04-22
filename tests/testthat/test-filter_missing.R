library(testthat)
library(recipes)

dat <- data.frame(
  dbl1 = rep(c(NA, 1), times = c(0, 100)),
  dbl2 = rep(c(NA, 1), times = c(25, 75)),
  dbl3 = rep(c(NA, 1), times = c(50, 50)),
  dbl4 = rep(c(NA, 1), times = c(75, 25)),
  dbl5 = rep(c(NA, 1), times = c(100, 0)),
  chr1 = rep(c(NA, "A"), times = c(10, 90)),
  chr2 = rep(c(NA, "A"), times = c(90, 10))
)

test_that("high filter", {
  rec <- recipe(~., data = dat)
  filtering <- rec |>
    step_filter_missing(all_predictors(), threshold = .2)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that("low filter", {
  rec <- recipe(~., data = dat)
  filtering <- rec |>
    step_filter_missing(all_predictors(), threshold = 0.8)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  expect_equal(filtering_trained$steps[[1]]$removals, c("dbl5", "chr2"))
})

test_that("Remove all columns with missing data", {
  rec <- recipe(~., data = dat)
  filtering <- rec |>
    step_filter_missing(all_predictors(), threshold = 0)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr1", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_filter_missing(all_predictors())
  rec_param <- tunable.step_filter_missing(rec$steps[[1]])
  expect_equal(rec_param$name, c("threshold"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("case weights", {
  dat_cw <- dat |>
    mutate(wts = frequency_weights(rep(c(1, 0), c(20, 80))))

  rec <- recipe(~., data = dat_cw)
  filtering <- rec |>
    step_filter_missing(all_predictors(), threshold = .2)

  filtering_trained <- prep(filtering)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr1", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)

  expect_snapshot(filtering_trained)

  # ----------------------------------------------------------------------------

  dat_cw <- dat |>
    mutate(wts = importance_weights(rep(c(1, 0), c(20, 80))))

  rec <- recipe(~., data = dat_cw)
  filtering <- rec |>
    step_filter_missing(all_predictors(), threshold = .2)

  filtering_trained <- prep(filtering)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)

  expect_snapshot(filtering_trained)
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_double(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)
  rec <- recipe(~ am + vs, data = mtcars) |>
    step_scale(am, vs)

  rec_trained <- prep(rec, training = mtcars, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = mtcars)

  expect_true(all(vapply(rec_trans, sparsevctrs::is_sparse_double, logical(1))))

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_filter_missing() removes variables and thus does not care if they are
  # not there.
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_filter_missing(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_filter_missing(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_filter_missing(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = dat) |>
    step_filter_missing(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_filter_missing(
      all_predictors(),
      threshold = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = dat) |>
      step_filter_missing(all_predictors(), threshold = -.2) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_filter_missing(all_predictors()) |>
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
