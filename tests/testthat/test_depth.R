library(testthat)
library(recipes)

test_that("defaults", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species", metric = "spatial", id = "")
  trained <- prep(rec, training = iris, verbose = FALSE)
  depths <- bake(trained, new_data = iris)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(iris[, 1:4], iris$Species)
  spatial <- function(x, y) {
    ddalpha::depth.spatial(x = y, data = x)
  }

  exp_res <- lapply(split_up, spatial, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for (i in 1:ncol(exp_res)) {
    expect_equal(depths[, i], exp_res[, i])
  }

  depth_tibble_un <-
    tibble(
      terms = "all_predictors()",
      class = NA_character_,
      id = ""
    )
  depth_tibble_tr <-
    tibble(
      terms = names(iris)[1:4],
      class = rep("Species", 4),
      id = ""
    )

  expect_equal(tidy(rec, 1), depth_tibble_un)
  expect_equal(tidy(trained, 1), depth_tibble_tr)
})

test_that("alt args", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(),
      class = "Species",
      metric = "Mahalanobis",
      options = list(mah.estimate = "MCD", mah.parMcd = .75)
    )
  trained <- prep(rec, training = iris, verbose = FALSE)
  depths <- bake(trained, new_data = iris)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(iris[, 1:4], iris$Species)
  Mahalanobis <- function(x, y) {
    ddalpha::depth.Mahalanobis(x = y, data = x, mah.estimate = "MCD", mah.parMcd = .75)
  }

  exp_res <- lapply(split_up, Mahalanobis, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  head(exp_res)
  head(depths)

  for (i in 1:ncol(exp_res)) {
    expect_equal(depths[, i], exp_res[, i])
  }
})


test_that("printing", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(), class = "Species", metric = "spatial")
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("prefix", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(all_predictors(),
      class = "Species",
      metric = "spatial", prefix = "spatial_"
    )
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  expect_false(any(grepl("depth_", names(dists))))
  expect_true(any(grepl("spatial_", names(dists))))
})

test_that("empty selection prep/bake adds NA columns", {
  skip_if_not_installed("ddalpha")
  rec1 <- recipe(Species ~ ., iris)
  rec2 <- step_depth(rec1, class = "Species")

  rec2 <- prep(rec2, iris)

  baked2 <- bake(rec2, iris)

  expect_identical(baked2$depth_setosa, rep(NA_real_, nrow(iris)))
  expect_identical(baked2$depth_versicolor, rep(NA_real_, nrow(iris)))
  expect_identical(baked2$depth_virginica, rep(NA_real_, nrow(iris)))
})

test_that("empty selection tidy method works", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., iris)
  rec <- step_depth(rec, class = "Species")

  expect <- tibble(terms = character(), class = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, iris)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if_not_installed("ddalpha")
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(Species ~ ., iris)
  rec <- step_depth(rec, class = "Species")

  expect_snapshot(rec)

  rec <- prep(rec, iris)

  expect_snapshot(rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("ddalpha")

  rec <- recipe(Species ~ ., data = iris) %>%
    step_depth(starts_with("Sepal"), class = "Species", metric = "spatial") %>%
    update_role(starts_with("Sepal"), new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = iris, verbose = FALSE)

  expect_error(bake(trained, new_data = iris[, 2:5]),
               class = "new_data_missing_column")
})
