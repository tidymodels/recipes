library(testthat)
library(recipes)

context("class distances")

# Note: some tests convert to data frame prior to testing
# https://github.com/tidyverse/dplyr/issues/2751

eps <- if (capabilities("long.double"))
  sqrt(.Machine$double.eps) else
  0.1

test_that("defaults", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE, id = "")
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  dists <- dists[, grepl("classdist", names(dists))]
  dists <- as.data.frame(dists)

  split_up <- split(iris[, 1:4], iris$Species)
  mahalanobis2 <- function(x, y)
    mahalanobis(y, center = colMeans(x), cov = cov(x))

  exp_res <- lapply(split_up, mahalanobis2, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(dists[, i], exp_res[, i])

  tidy_exp_un <- tibble(
    terms = "all_predictors()",
    value = NA_real_,
    class = NA_character_,
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(rec, number = 1))
  means <- lapply(split_up, colMeans)
  means <- unlist(unname(means))

  tidy_exp_tr <- tibble(
    terms = names(means),
    value = unname(means),
    class = rep(names(split_up), each = 4),
    id = ""
  )
  expect_equal(
    as.data.frame(tidy_exp_tr),
    as.data.frame(tidy(trained, number = 1)),
    tolerance = eps
  )
})

test_that("alt args", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE, mean_func = median)
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
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
  expect_output(prep(rec, training = iris, verbose = TRUE))
})
