library(testthat)
library(recipes)

context("correlation filter")


n <- 100
set.seed(424)
dat <- matrix(rnorm(n*5), ncol =  5)
dat <- as.data.frame(dat)
dat$duplicate <- dat$V1
dat$V6 <- -dat$V2 + runif(n)*.2

test_that('high filter', {
  set.seed(1)
  rec <- recipe(~ ., data = dat)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .5)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  removed <- c("V6", "V1")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that('low filter', {
  rec <- recipe(~ ., data = dat)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = 1)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  expect_equal(filtering_trained$steps[[1]]$removals, numeric(0))
})

test_that('many missing values', {
  dat2 <- dat
  dat2$V4 <- NA_real_
  rec <- recipe(~ ., data = dat2)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .25)

  filtering_trained <-
    expect_warning(
      prep(filtering, training = dat2, verbose = FALSE),
      "1 columns were excluded from the filter"
    )

  expect_equal(filtering_trained$steps[[1]]$removals, paste0("V", 1:2))
})

test_that('occasional missing values', {
  dat3 <- dat
  dat3$V1[1] <- NA_real_
  dat3$V4[10] <- NA_real_
  rec <- recipe(~ ., data = dat3)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .25, use = "everything")

  filtering_trained <-
    expect_warning(
      prep(filtering, training = dat3, verbose = FALSE),
      "Some columns were excluded from the filter"
    )

  expect_equal(filtering_trained$steps[[1]]$removals, "V2")
})


test_that('printing', {
  set.seed(1)
  rec <- recipe(~ ., data = dat)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .5)
  expect_output(print(filtering))
  expect_output(prep(filtering, training = dat, verbose = TRUE))
})

