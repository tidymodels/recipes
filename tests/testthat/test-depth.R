library(testthat)
library(recipes)

test_that("defaults", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., data = iris) |>
    step_depth(all_predictors(), class = Species, metric = "spatial", id = "")
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
  rec <- recipe(Species ~ ., data = iris) |>
    step_depth(
      all_predictors(),
      class = Species,
      metric = "Mahalanobis",
      options = list(mah.estimate = "MCD", mah.parMcd = .75)
    )
  trained <- prep(rec, training = iris, verbose = FALSE)
  depths <- bake(trained, new_data = iris)
  depths <- depths[, grepl("depth", names(depths))]
  depths <- as.data.frame(depths)

  split_up <- split(iris[, 1:4], iris$Species)
  Mahalanobis <- function(x, y) {
    ddalpha::depth.Mahalanobis(
      x = y,
      data = x,
      mah.estimate = "MCD",
      mah.parMcd = .75
    )
  }

  exp_res <- lapply(split_up, Mahalanobis, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  head(exp_res)
  head(depths)

  for (i in 1:ncol(exp_res)) {
    expect_equal(depths[, i], exp_res[, i])
  }
})

test_that("prefix", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., data = iris) |>
    step_depth(
      all_predictors(),
      class = Species,
      metric = "spatial",
      prefix = "spatial_"
    )
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  expect_false(any(grepl("depth_", names(dists))))
  expect_true(any(grepl("spatial_", names(dists))))
})

test_that("bad args", {
  skip_if_not_installed("ddalpha")
  expect_snapshot(
    recipe(Species ~ ., data = iris) |>
      step_depth(
        all_numeric_predictors(),
        class = Species,
        metric = "circular"
      ) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(Species ~ ., data = iris) |>
      step_depth(all_numeric_predictors(), class = Species, prefix = 0L) |>
      prep(),
    error = TRUE
  )
})

test_that("check_options() is used", {
  skip_if_not_installed("ddalpha")
  expect_snapshot(
    error = TRUE,
    recipe(~Species, data = iris) |>
      step_depth(
        all_numeric_predictors(),
        class = Species,
        options = TRUE
      ) |>
      prep()
  )
})

test_that("recipes_argument_select() is used", {
  skip_if_not_installed("ddalpha")

  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_depth(disp, class = NULL) |>
      prep()
  )
})

test_that("addition of recipes_argument_select() is backwards compatible", {
  skip_if_not_installed("ddalpha")

  rec <- recipe(Species ~ ., data = iris) |>
    step_depth(all_predictors(), class = Species, options = list(seed = 1)) |>
    prep()

  exp <- bake(rec, iris)

  rec$steps[[1]]$class <- "Species"

  expect_identical(
    bake(rec, iris),
    exp
  )

  rec_old <- recipe(Species ~ ., data = iris) |>
    step_depth(
      all_predictors(),
      class = "Species",
      options = list(seed = 1)
    ) |>
    prep()

  expect_identical(
    bake(rec_old, iris),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("ddalpha")

  rec <- recipe(Species ~ ., data = iris) |>
    step_depth(starts_with("Sepal"), class = Species, metric = "spatial") |>
    update_role(starts_with("Sepal"), new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = iris, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(trained, new_data = iris[, 2:5]))
})

test_that("empty printing", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., iris)
  rec <- step_depth(rec, class = Species)

  expect_snapshot(rec)

  rec <- prep(rec, iris)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  skip_if_not_installed("ddalpha")
  rec1 <- recipe(Species ~ ., iris)
  rec2 <- step_depth(rec1, class = Species)

  rec1 <- prep(rec1, iris)
  rec2 <- prep(rec2, iris)

  baked1 <- bake(rec1, iris)
  baked2 <- bake(rec2, iris)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., iris)
  rec <- step_depth(rec, class = Species)

  expect <- tibble(terms = character(), class = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, iris)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("ddalpha")
  new_names <- c(
    "Species",
    "depth_setosa",
    "depth_versicolor",
    "depth_virginica"
  )

  rec <- recipe(Species ~ ., iris) |>
    step_depth(all_predictors(), class = Species, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(Species ~ ., iris) |>
    step_depth(all_predictors(), class = Species, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    unique(c(names(iris), new_names))
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., iris) |>
    step_depth(all_predictors(), class = Species)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = iris)
  )
})

test_that("printing", {
  skip_if_not_installed("ddalpha")
  rec <- recipe(Species ~ ., data = iris) |>
    step_depth(all_predictors(), class = Species, metric = "spatial")

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  skip_if_not_installed("ddalpha")
  data <- iris
  rec <- recipe(~., data) |>
    step_depth(all_numeric_predictors(), class = Species) |>
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
