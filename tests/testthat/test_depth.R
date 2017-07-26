library(testthat)
library(recipes)
library(ddalpha)

test_that("defaults", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species", metric = "spatial")
  trained <- prep(rec, training = iris, verbose = FALSE)
  depths <- bake(trained, newdata = iris)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(iris[, 1:4], iris$Species)
  spatial <- function(x, y)
    depth.spatial(x = y, data = x)

  exp_res <- lapply(split_up, spatial, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(depths[, i], exp_res[, i])
})

test_that("alt args", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species",
               metric = "Mahalanobis",
               options = list(mah.estimate = "MCD", mah.parMcd = .75))
  trained <- prep(rec, training = iris, verbose = FALSE)
  depths <- bake(trained, newdata = iris)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(iris[, 1:4], iris$Species)
  Mahalanobis <- function(x, y)
    depth.Mahalanobis(x = y, data = x, mah.estimate = "MCD", mah.parMcd = .75)

  exp_res <- lapply(split_up, Mahalanobis, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  head(exp_res)
  head(depths)

  for(i in 1:ncol(exp_res))
    expect_equal(depths[, i], exp_res[, i])
})


test_that('printing', {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species", metric = "spatial")
  expect_output(print(rec))
  expect_output(prep(rec, training = iris))
})

