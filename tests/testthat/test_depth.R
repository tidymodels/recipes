library(testthat)
library(recipes)
library(ddalpha)

context("depth features")


test_that("defaults", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species", metric = "spatial", id = "")
  trained <- prep(rec, training = iris, verbose = FALSE)
  depths <- bake(trained, new_data = iris)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(iris[, 1:4], iris$Species)
  spatial <- function(x, y)
    depth.spatial(x = y, data = x)

  exp_res <- lapply(split_up, spatial, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(depths[, i], exp_res[, i])

  depth_tibble_un <-
    tibble(terms = "all_predictors()",
           class = NA_character_,
           id = "")
  depth_tibble_tr <-
    tibble(terms = names(iris)[1:4],
           class = rep("Species", 4),
           id = "")

  expect_equal(tidy(rec, 1), depth_tibble_un)
  expect_equal(tidy(trained, 1), depth_tibble_tr)

})

test_that("alt args", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species",
               metric = "Mahalanobis",
               options = list(mah.estimate = "MCD", mah.parMcd = .75))
  trained <- prep(rec, training = iris, verbose = FALSE)
  depths <- bake(trained, new_data = iris)
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
  expect_output(prep(rec, training = iris, verbose = TRUE))
})

test_that('prefix', {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species",
               metric = "spatial", prefix = "spatial_")
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  expect_false(any(grepl("depth_", names(dists))))
  expect_true(any(grepl("spatial_", names(dists))))
})
