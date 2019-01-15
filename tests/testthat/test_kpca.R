library(testthat)
library(kernlab)
library(recipes)

context("Kernel PCA")


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
    step_kpca(X2, X3, X4, X5, X6, id = "")

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- kpca(as.matrix(tr_dat[, -1]),
                  kernel = kpca_rec$steps[[1]]$options$kernel,
                  kpar = kpca_rec$steps[[1]]$options$kpar)

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


test_that('deprecated arg', {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("kernlab")
  
  expect_message(
    rec %>%
      step_kpca(X2, X3, X4, X5, X6, num = 2)
  )
})

test_that('printing', {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("kernlab")
  
  kpca_rec <- rec %>%
    step_kpca(X2, X3, X4, X5, X6)
  expect_output(print(kpca_rec))
  expect_output(prep(kpca_rec, training = tr_dat, verbose = TRUE))
})

