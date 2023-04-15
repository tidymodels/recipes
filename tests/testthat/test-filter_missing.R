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
  filtering <- rec %>%
    step_filter_missing(all_predictors(), threshold = .2)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that("low filter", {
  rec <- recipe(~., data = dat)
  filtering <- rec %>%
    step_filter_missing(all_predictors(), threshold = 0.8)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  expect_equal(filtering_trained$steps[[1]]$removals, c("dbl5", "chr2"))
})

test_that("Remove all columns with missing data", {
  rec <- recipe(~., data = dat)
  filtering <- rec %>%
    step_filter_missing(all_predictors(), threshold = 0)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr1", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that("printing", {
  set.seed(1)
  rec <- recipe(~., data = dat)
  filtering <- rec %>%
    step_filter_missing(all_predictors(), threshold = .5)
  expect_snapshot(print(filtering))
  expect_snapshot(prep(filtering))
})


test_that("tunable", {
  rec <-
    recipe(~., data = iris) %>%
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

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_filter_missing(
      all_predictors(),
      threshold = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
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

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_filter_missing(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("case weights", {
  dat_cw <- dat %>%
    mutate(wts = frequency_weights(rep(c(1, 0), c(20, 80))))

  rec <- recipe(~., data = dat_cw)
  filtering <- rec %>%
    step_filter_missing(all_predictors(), threshold = .2)

  filtering_trained <- prep(filtering)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr1", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)

  expect_snapshot(filtering_trained)

  # ----------------------------------------------------------------------------

  dat_cw <- dat %>%
    mutate(wts = importance_weights(rep(c(1, 0), c(20, 80))))

  rec <- recipe(~., data = dat_cw)
  filtering <- rec %>%
    step_filter_missing(all_predictors(), threshold = .2)

  filtering_trained <- prep(filtering)

  removed <- c("dbl2", "dbl3", "dbl4", "dbl5", "chr2")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)

  expect_snapshot(filtering_trained)
})
