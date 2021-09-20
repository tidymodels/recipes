library(testthat)
library(kernlab)
library(recipes)

set.seed(131)
tr_dat <- matrix(rnorm(100*6), ncol = 6)
te_dat <- matrix(rnorm(20*6), ncol = 6)
colnames(tr_dat) <- paste0("X", 1:6)
colnames(te_dat) <- paste0("X", 1:6)

rec <- recipe(X1 ~ ., data = tr_dat)

test_that('correct kernel PCA values', {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>%
    step_kpca_rbf(X2, X3, X4, X5, X6, id = "")

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- kpca(as.matrix(tr_dat[, -1]),
                  kernel = "rbfdot",
                  kpar = list(sigma = 0.2))

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

test_that('printing', {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>%
    step_kpca_rbf(X2, X3, X4, X5, X6)
  expect_snapshot(kpca_rec)
  expect_snapshot(prep(kpca_rec, training = tr_dat, verbose = TRUE))
})


test_that('No kPCA comps', {
  pca_extract <- rec %>%
    step_kpca_rbf(X2, X3, X4, X5, X6, num_comp = 0, id = "") %>%
    prep()

  expect_equal(
    names(juice(pca_extract)),
    paste0("X", c(2:6, 1))
  )
  expect_true(inherits(pca_extract$steps[[1]]$res, "list"))
  expect_snapshot(pca_extract)
  expect_equal(
    tidy(pca_extract, 1),
    tibble::tibble(terms = paste0("X", 2:6), id = "")
  )
})


test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_kpca_rbf(all_predictors())
  rec_param <- tunable.step_kpca_rbf(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp", "sigma"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})

test_that('keep_original_cols works', {

  skip_if_not_installed("dimRed")
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>%
    step_kpca_rbf(X2, X3, X4, X5, X6, id = "", keep_original_cols = TRUE)

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors())

  expect_equal(
    colnames(pca_pred),
    c("X2", "X3", "X4", "X5", "X6",
      "kPC1", "kPC2", "kPC3", "kPC4", "kPC5")
  )

})

test_that('can prep recipes with no keep_original_cols', {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("kernlab")

  kpca_rec <- rec %>%
    step_kpca_poly(X2, X3, X4, X5, X6, id = "")

  kpca_rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE),
  )

  expect_error(
    pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors()),
    NA
  )

})
