library(testthat)
library(recipes)

test_that("defaults", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE)
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, newdata = iris)
  dists <- dists[, grepl("classdist", names(dists))]
  dists <- as.data.frame(dists)

  split_up <- split(iris[, 1:4], iris$Species)
  mahalanobis2 <- function(x, y)
    mahalanobis(y, center = colMeans(x), cov = cov(x))

  exp_res <- lapply(split_up, mahalanobis2, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(dists[, i], exp_res[, i])
})

test_that("alt args", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE, mean_func = median)
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, newdata = iris)
  dists <- dists[, grepl("classdist", names(dists))]
  dists <- as.data.frame(dists)

  split_up <- split(iris[, 1:4], iris$Species)
  mahalanobis2 <- function(x, y)
    mahalanobis(y, center = apply(x, 2, median), cov = cov(x))

  exp_res <- lapply(split_up, mahalanobis2, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(dists[, i], exp_res[, i])
})

test_that('printing', {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris))
})
