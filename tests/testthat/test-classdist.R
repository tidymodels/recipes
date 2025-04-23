library(testthat)
library(recipes)

# Note: some tests convert to data frame prior to testing
# https://github.com/tidyverse/dplyr/issues/2751

eps <- if (capabilities("long.double")) {
  sqrt(.Machine$double.eps)
} else {
  0.1
}

test_that("defaults", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist(all_predictors(), class = Species, log = FALSE, id = "")
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  dists <- dists[, grepl("classdist", names(dists))]
  dists <- as.data.frame(dists)

  split_up <- split(iris[, 1:4], iris$Species)
  mahalanobis2 <- function(x, y) {
    mahalanobis(y, center = colMeans(x), cov = cov(x))
  }

  exp_res <- lapply(split_up, mahalanobis2, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for (i in 1:ncol(exp_res)) {
    expect_equal(dists[, i], exp_res[, i])
  }

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
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist(
      all_predictors(),
      class = Species,
      log = FALSE,
      mean_func = median
    )
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  dists <- dists[, grepl("classdist", names(dists))]
  dists <- as.data.frame(dists)

  split_up <- split(iris[, 1:4], iris$Species)
  mahalanobis2 <- function(x, y) {
    mahalanobis(y, center = apply(x, 2, median), cov = cov(x))
  }

  exp_res <- lapply(split_up, mahalanobis2, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for (i in 1:ncol(exp_res)) {
    expect_equal(dists[, i], exp_res[, i])
  }
})

test_that("check_name() is used", {
  dat <- iris
  dat$classdist_setosa <- dat$Sepal.Length

  rec <- recipe(Species ~ ., data = dat) |>
    step_classdist(
      Sepal.Length,
      Sepal.Width,
      Petal.Length,
      Petal.Width,
      class = Species
    )

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("prefix", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist(
      all_predictors(),
      class = Species,
      log = FALSE,
      prefix = "centroid_"
    )
  trained <- prep(rec, training = iris, verbose = FALSE)
  dists <- bake(trained, new_data = iris)
  expect_false(any(grepl("classdist_", names(dists))))
  expect_true(any(grepl("centroid_", names(dists))))
})

test_that("case weights", {
  set.seed(1)
  wts <- runif(32)

  means_exp <- colMeans(mtcars)
  means_wts <- recipes:::get_center(mtcars, wts = wts)
  means_no <- recipes:::get_center(mtcars)
  means_wts_exp <- purrr::map_dbl(mtcars, weighted.mean, w = wts)

  expect_equal(means_wts, means_wts_exp)
  expect_equal(means_no, means_exp)
  expect_snapshot(
    error = TRUE,
    recipes:::get_center(mtcars, wts = wts, mfun = median)
  )

  # ------------------------------------------------------------------------------

  cov_exp <- cov(mtcars)
  cov_wts <- recipes:::get_both(mtcars, wts = wts)
  cov_no <- recipes:::get_both(mtcars)
  cov_wts_exp <- cov.wt(mtcars, wt = wts)$cov
  expect_equal(cov_wts$scale, cov_wts_exp)
  expect_equal(cov_no$scale, cov_exp)
  expect_equal(cov_wts$center, means_wts_exp)
  expect_equal(cov_no$center, means_exp)
  expect_snapshot(
    error = TRUE,
    recipes:::get_both(mtcars, wts = wts, mfun = median)
  )
  expect_snapshot(
    error = TRUE,
    recipes:::get_both(mtcars, wts = wts, cfun = mad)
  )

  # ------------------------------------------------------------------------------

  iris1 <- iris
  iris1$wts <- importance_weights(iris1$Petal.Width)

  rec_prep <- recipe(Species ~ ., data = iris1) |>
    step_classdist(all_predictors(), class = Species) |>
    prep()

  ref_objects <- split(iris1, ~Species) |>
    purrr::map(
      ~ get_both(.x |> select(-Species, -wts), wts = as.numeric(.x$wts))
    )

  expect_equal(
    rec_prep$steps[[1]]$objects,
    ref_objects
  )

  rec_prep <- recipe(Species ~ ., data = iris1) |>
    step_classdist(all_predictors(), class = Species, pool = TRUE) |>
    prep()

  ref_objects_means <- split(iris1, ~Species) |>
    purrr::map(
      ~ averages(.x |> select(-Species, -wts), wts = as.numeric(.x$wts))
    )

  ref_object_cov <- covariances(iris1[1:4], wts = iris1$wts)

  expect_equal(
    rec_prep$steps[[1]]$objects,
    list(center = ref_objects_means, scale = ref_object_cov)
  )

  expect_snapshot(rec_prep)
})

test_that("recipes_argument_select() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_classdist(disp, class = NULL) |>
      prep()
  )
})

test_that("addition of recipes_argument_select() is backwards compatible", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist(all_predictors(), class = Species) |>
    prep()

  exp <- bake(rec, iris)

  rec$steps[[1]]$class <- "Species"

  expect_identical(
    bake(rec, iris),
    exp
  )

  rec_old <- recipe(Species ~ ., data = iris) |>
    step_classdist(all_predictors(), class = "Species") |>
    prep()

  expect_identical(
    bake(rec_old, iris),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist(Petal.Length, class = Species, log = FALSE) |>
    update_role(Petal.Length, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = iris, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(trained, new_data = iris[, c(-3)]))
})

test_that("empty printing", {
  rec <- recipe(Species ~ ., iris)
  rec <- step_classdist(rec, class = Species)

  expect_snapshot(rec)

  rec <- prep(rec, iris)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_classdist(rec1, class = mpg)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(Species ~ ., iris)
  rec2 <- step_classdist(rec, class = Species, pool = FALSE)
  rec3 <- step_classdist(rec, class = Species, pool = TRUE)

  expect <- tibble(
    terms = character(),
    value = double(),
    class = character(),
    id = character()
  )

  expect_identical(tidy(rec2, number = 1), expect)
  expect_identical(tidy(rec3, number = 1), expect)

  rec2 <- prep(rec2, iris)
  rec3 <- prep(rec3, iris)

  expect_identical(tidy(rec2, number = 1), expect)
  expect_identical(tidy(rec3, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c(
    "Species",
    "classdist_setosa",
    "classdist_versicolor",
    "classdist_virginica"
  )

  rec <- recipe(Species ~ Sepal.Length, data = iris) |>
    step_classdist(
      all_predictors(),
      class = Species,
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(Species ~ Sepal.Length, data = iris) |>
    step_classdist(
      all_predictors(),
      class = Species,
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("Sepal.Length", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(Species ~ Sepal.Length, data = iris) |>
    step_classdist(all_predictors(), class = Species)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = iris)
  )
})

test_that("printing", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist(all_predictors(), class = Species)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(Species ~ ., data = iris) |>
      step_classdist(all_predictors(), class = Species, mean_func = 2) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(Species ~ ., data = iris) |>
      step_classdist(all_predictors(), class = Species, cov_func = NULL) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(Species ~ ., data = iris) |>
      step_classdist(all_predictors(), class = Species, prefix = NULL) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(Species ~ ., data = iris) |>
      step_classdist(all_predictors(), class = Species, pool = NULL) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_classdist(all_numeric_predictors(), class = Species) |>
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
