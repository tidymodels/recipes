library(testthat)
library(recipes)
library(ddalpha)
library(modeldata)
data(scat)
scat <- na.omit(scat)

context("depth features")


test_that("defaults", {
  rec <- recipe(Species ~ Age + Mass + Length, data = scat) %>%
    step_depth(all_predictors(), class = "Species", metric = "spatial", id = "")
  trained <- prep(rec, training = scat, verbose = FALSE)
  depths <- bake(trained, new_data = scat)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(scat[, c(6, 12, 8)], scat$Species)
  spatial <- function(x, y)
    depth.spatial(x = y, data = x)

  exp_res <- lapply(split_up, spatial, y = scat[, c(6, 12, 8)])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(depths[, i], exp_res[, i])

  depth_tibble_un <-
    tibble(terms = "all_predictors()",
           class = NA_character_,
           id = "")
  depth_tibble_tr <-
    tibble(terms = names(scat)[c(6, 12, 8)],
           class = rep("Species", 3),
           id = "")

  expect_equal(tidy(rec, 1), depth_tibble_un)
  expect_equal(tidy(trained, 1), depth_tibble_tr)

})

test_that("alt args", {
  rec <- recipe(Species ~ Age + Mass + Length, data = scat) %>%
    step_depth(all_predictors(), class = "Species",
               metric = "Mahalanobis",
               options = list(mah.estimate = "MCD", mah.parMcd = .75))
  trained <- prep(rec, training = scat, verbose = FALSE)
  depths <- bake(trained, new_data = scat)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(scat[, c(6, 12, 8)], scat$Species)
  Mahalanobis <- function(x, y)
    depth.Mahalanobis(x = y, data = x, mah.estimate = "MCD", mah.parMcd = .75)

  exp_res <- lapply(split_up, Mahalanobis, y = scat[, c(6, 12, 8)])
  exp_res <- as.data.frame(exp_res)

  head(exp_res)
  head(depths)

  for(i in 1:ncol(exp_res))
    expect_equal(depths[, i], exp_res[, i])
})


test_that('printing', {
  rec <- recipe(Species ~ Age + Mass + Length, data = scat) %>%
    step_depth(all_predictors(), class = "Species", metric = "spatial")
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})

test_that('prefix', {
  rec <- recipe(Species ~ Age + Mass + Length, data = scat) %>%
    step_depth(all_predictors(), class = "Species",
               metric = "spatial", prefix = "spatial_")
  trained <- prep(rec, training = scat, verbose = FALSE)
  dists <- bake(trained, new_data = scat)
  expect_false(any(grepl("depth_", names(dists))))
  expect_true(any(grepl("spatial_", names(dists))))
})
