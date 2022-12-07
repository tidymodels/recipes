library(testthat)
library(recipes)


set.seed(131)
tr_dat <- matrix(rnorm(100 * 6), ncol = 6)
te_dat <- matrix(rnorm(20 * 6), ncol = 6)
colnames(tr_dat) <- paste0("X", 1:6)
colnames(te_dat) <- paste0("X", 1:6)

rec <- recipe(X1 ~ ., data = tr_dat)

test_that("correct kernel PCA values", {
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6, id = "")

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- kernlab::kpca(as.matrix(tr_dat[, -1]),
    kernel = kpca_rec$steps[[1]]$options$kernel,
    kpar = kpca_rec$steps[[1]]$options$kpar
  )

  pca_pred_exp <- kernlab::predict(pca_exp, te_dat[, -1])[, 1:kpca_trained$steps[[1]]$num_comp]
  colnames(pca_pred_exp) <- paste0("kPC", 1:kpca_trained$steps[[1]]$num_comp)

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)

  kpca_tibble <-
    tibble(terms = c("X2", "X3", "X4", "X5", "X6"), id = "")

  expect_equal(tidy(kpca_rec, 1), kpca_tibble)
  expect_equal(tidy(kpca_trained, 1), kpca_tibble)
})


test_that("printing", {
  skip_if_not_installed("kernlab")

  skip_if(packageVersion("rlang") < "1.0.0")
  expect_snapshot(
    kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6)
  )

  expect_snapshot(kpca_rec)
  expect_snapshot(prep(kpca_rec))
})


test_that("No kPCA comps", {
  skip_if_not_installed("kernlab")
  suppressWarnings(
    pca_extract <- rec %>%
      step_kpca(X2, X3, X4, X5, X6, num_comp = 0, id = "") %>%
      prep()
  )

  expect_equal(
    names(bake(pca_extract, new_data = NULL)),
    paste0("X", c(2:6, 1))
  )
  expect_null(pca_extract$steps[[1]]$res)
  expect_equal(
    tidy(pca_extract, 1),
    tibble::tibble(terms = paste0("X", 2:6), id = "")
  )
  skip_if(packageVersion("rlang") < "1.0.0")
  expect_snapshot(
    pca_extract <- rec %>%
      step_kpca(X2, X3, X4, X5, X6, num_comp = 0, id = "") %>%
      prep()
  )
  expect_snapshot(pca_extract)
})


test_that("keep_original_cols works", {
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>%
    step_kpca(X2, X3, X4, X5, X6, id = "", keep_original_cols = TRUE)

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors())

  expect_equal(
    colnames(pca_pred),
    c(
      "X2", "X3", "X4", "X5", "X6",
      "kPC1", "kPC2", "kPC3", "kPC4", "kPC5"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>%
    step_kpca(X2, X3, X4, X5, X6, id = "")

  kpca_rec$steps[[1]]$keep_original_cols <- NULL

  suppressWarnings(
    kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)
  )

  expect_error(
    pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors()),
    NA
  )
  skip_if(packageVersion("rlang") < "1.0.0")
  expect_snapshot(
    kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE),
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_kpca(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_kpca(rec, id = "potato")

  expected <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expected)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expected)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_kpca(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})


test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6) %>%
    update_role(X2, X3, X4, X5, X6, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  expect_error(bake(kpca_trained, new_data = te_dat[, 1:3]),
               class = "new_data_missing_column")
})
