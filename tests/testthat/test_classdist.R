library(testthat)
library(recipes)

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

test_that('prefix', {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species",
                   log = FALSE, prefix = "centroid_")
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  expect_false(any(grepl("classdist_", names(dists))))
  expect_true(any(grepl("centroid_", names(dists))))
})

test_that("empty selection prep/bake returns NA columns", {
  rec1 <- recipe(Species ~ ., iris)
  rec2 <- step_classdist(rec1, class = "Species", pool = FALSE)
  rec3 <- step_classdist(rec1, class = "Species", pool = TRUE)

  rec2 <- prep(rec2, iris)
  rec3 <- prep(rec3, iris)

  baked2 <- bake(rec2, iris)
  baked3 <- bake(rec3, iris)

  expect <- rep(NA_real_, nrow(iris))

  expect_identical(baked2$classdist_setosa, expect)
  expect_identical(baked2$classdist_versicolor, expect)
  expect_identical(baked2$classdist_virginica, expect)

  expect_identical(baked3$classdist_setosa, expect)
  expect_identical(baked3$classdist_versicolor, expect)
  expect_identical(baked3$classdist_virginica, expect)
})

test_that("empty selection tidy method works", {
  rec <- recipe(Species ~ ., iris)
  rec2 <- step_classdist(rec, class = "Species", pool = FALSE)
  rec3 <- step_classdist(rec, class = "Species", pool = TRUE)

  expect <- tibble(terms = character(), value = double(), class = character(), id = character())

  expect_identical(tidy(rec2, number = 1), expect)
  expect_identical(tidy(rec3, number = 1), expect)

  rec2 <- prep(rec2, iris)
  rec3 <- prep(rec3, iris)

  expect_identical(tidy(rec2, number = 1), expect)
  expect_identical(tidy(rec3, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(Species ~ ., iris)
  rec <- step_classdist(rec, class = "Species")

  expect_snapshot(rec)

  rec <- prep(rec, iris)

  expect_snapshot(rec)
})
