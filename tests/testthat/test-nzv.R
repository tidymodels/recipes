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

test_that("nzv filtering", {
  rec <- recipe(y ~ ., data = dat)
  filtering <- rec |>
    step_nzv(x1, x2, x3, x4, id = "")

  exp_tidy_un <- tibble(terms = c("x1", "x2", "x3", "x4"), id = "")
  expect_equal(exp_tidy_un, tidy(filtering, number = 1))

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  removed <- vars[
    pct_uni <= filtering_trained$steps[[1]]$unique_cut &
      f_ratio >= filtering_trained$steps[[1]]$freq_cut
  ]

  exp_tidy_tr <- tibble(terms = removed, id = "")
  expect_equal(exp_tidy_tr, tidy(filtering_trained, number = 1))

  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that("altered freq_cut and unique_cut", {
  rec <- recipe(y ~ ., data = dat)

  filtering <- rec |>
    step_nzv(x1, x2, x3, x4, freq_cut = 50, unique_cut = 10)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  removed <- vars[
    pct_uni <= filtering_trained$steps[[1]]$unique_cut &
      f_ratio >= filtering_trained$steps[[1]]$freq_cut
  ]

  expect_equal(filtering_trained$steps[[1]]$removals, removed)

  expect_snapshot_error(
    rec |>
      step_nzv(x1, x2, x3, x4, options = list(freq_cut = 50, unique_cut = 10))
  )
})

test_that("Deprecation warning", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_nzv(options = list(freq_cut = 95 / 5, unique_cut = 20))
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_nzv(all_predictors())
  rec_param <- tunable.step_nzv(rec$steps[[1]])
  expect_equal(rec_param$name, c("freq_cut", "unique_cut"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("nzv with case weights", {
  weighted_int_counts <- dat |> count(x3, wt = x2, sort = TRUE)
  exp_freq_cut_int <- weighted_int_counts$n[1] / weighted_int_counts$n[2]

  dat_caseweights_x2 <- dat |>
    mutate(x2 = frequency_weights(x2))

  expect_equal(
    recipe(~., dat_caseweights_x2) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_int) |>
      prep() |>
      tidy(1) |>
      pull(terms),
    c("x4")
  )

  expect_equal(
    recipe(~., dat_caseweights_x2) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_int - 0.0001) |>
      prep() |>
      tidy(1) |>
      pull(terms),
    c("x3", "x4")
  )

  weighted_frag_counts <- dat |> count(x3, wt = y, sort = TRUE)
  exp_freq_cut_frag <- weighted_frag_counts$n[1] / weighted_frag_counts$n[2]

  expect_snapshot(
    recipe(~., dat_caseweights_x2) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_int) |>
      prep()
  )

  # ----------------------------------------------------------------------------

  weighted_int_counts <- dat |> count(x3, wt = x2, sort = TRUE)
  exp_freq_cut_int <- weighted_int_counts$n[1] / weighted_int_counts$n[2]

  dat_caseweights_x2 <- dat |>
    mutate(x2 = importance_weights(x2))

  expect_equal(
    recipe(~., dat_caseweights_x2) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_int) |>
      prep() |>
      tidy(1) |>
      pull(terms),
    c("x4")
  )

  expect_equal(
    recipe(~., dat_caseweights_x2) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_int - 0.0001) |>
      prep() |>
      tidy(1) |>
      pull(terms),
    c("x4")
  )

  weighted_frag_counts <- dat |> count(x3, wt = y, sort = TRUE)
  exp_freq_cut_frag <- weighted_frag_counts$n[1] / weighted_frag_counts$n[2]

  dat_caseweights_y <- dat |>
    mutate(y = importance_weights(y))

  expect_equal(
    recipe(~., dat_caseweights_y) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_frag) |>
      prep() |>
      tidy(1) |>
      pull(terms),
    c("x3", "x4")
  )

  expect_equal(
    recipe(~., dat_caseweights_y) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_frag - 0.0001) |>
      prep() |>
      tidy(1) |>
      pull(terms),
    c("x3", "x4")
  )

  expect_snapshot(
    recipe(~., dat_caseweights_y) |>
      step_nzv(all_predictors(), freq_cut = exp_freq_cut_frag - 0.0001) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_nzv() removes variables and thus does not care if they are not there.
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_nzv(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_nzv(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_nzv(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(y ~ ., data = dat) |>
    step_nzv(x1, x2, x3, x4)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_nzv(
      all_predictors(),
      freq_cut = hardhat::tune(),
      unique_cut = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(y ~ ., data = dat) |>
      step_nzv(x1, freq_cut = -1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(y ~ ., data = dat) |>
      step_nzv(x1, unique_cut = 101) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_nzv(all_predictors()) |>
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
