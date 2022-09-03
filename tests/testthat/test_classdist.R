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
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE, id = "")
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
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE, mean_func = median)
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

test_that("printing", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("prefix", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(all_predictors(),
      class = "Species",
      log = FALSE, prefix = "centroid_"
    )
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


test_that("case weights", {
  set.seed(1)
  wts <- runif(32)

  means_exp <- colMeans(mtcars)
  means_wts <- recipes:::get_center(mtcars, wts = wts)
  means_no  <- recipes:::get_center(mtcars)
  means_wts_exp <- purrr::map_dbl(mtcars, weighted.mean, w = wts)

  expect_equal(means_wts, means_wts_exp)
  expect_equal(means_no, means_exp)
  expect_error(
    recipes:::get_center(mtcars, wts = wts, mfun = median),
    "The centering function requested cannot be used with case weights"
  )

  # ------------------------------------------------------------------------------

  cov_exp <- cov(mtcars)
  cov_wts <- recipes:::get_both(mtcars, wts = wts)
  cov_no  <- recipes:::get_both(mtcars)
  cov_wts_exp <- cov.wt(mtcars, wt = wts)$cov
  expect_equal(cov_wts$scale, cov_wts_exp)
  expect_equal(cov_no$scale, cov_exp)
  expect_equal(cov_wts$center, means_wts_exp)
  expect_equal(cov_no$center, means_exp)
  expect_error(
    recipes:::get_both(mtcars, wts = wts, mfun = median),
    "The centering function requested cannot be used with case weights"
  )
  expect_error(
    recipes:::get_both(mtcars, wts = wts, cfun = mad),
    "The variance function requested cannot be used with case weights"
  )

  # ------------------------------------------------------------------------------

  iris1 <- iris
  iris1$wts <- importance_weights(iris1$Petal.Width)

  rec_prep <- recipe(Species ~ ., data = iris1) %>%
    step_classdist(all_predictors(), class = "Species") %>%
    prep()

  ref_objects <- split(iris1, ~Species) %>%
    purrr::map(~get_both(.x %>% select(-Species, -wts), wts = as.numeric(.x$wts)))

  expect_equal(
    rec_prep$steps[[1]]$objects,
    ref_objects
  )

  rec_prep <- recipe(Species ~ ., data = iris1) %>%
    step_classdist(all_predictors(), class = "Species", pool = TRUE) %>%
    prep()

  ref_objects_means <- split(iris1, ~Species) %>%
    purrr::map(~averages(.x %>% select(-Species, -wts), wts = as.numeric(.x$wts)))

  ref_object_cov <- covariances(iris1[1:4], wts = iris1$wts)

  expect_equal(
    rec_prep$steps[[1]]$objects,
    list(center = ref_objects_means, scale = ref_object_cov)
  )

  expect_snapshot(rec_prep)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(Species ~ ., data = iris) %>%
    step_classdist(Petal.Length, class = "Species", log = FALSE)  %>%
    update_role(Petal.Length, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = iris, verbose = FALSE)

  expect_error(bake(trained, new_data = iris[,c(-3)]),
               class = "new_data_missing_column")
})
