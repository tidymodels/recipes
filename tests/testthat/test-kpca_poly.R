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

  kpca_rec <- rec |>
    step_kpca_poly(X2, X3, X4, X5, X6, id = "", degree = 3, scale_factor = .1)

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  pca_pred <- bake(kpca_trained, new_data = te_dat, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- kernlab::kpca(
    as.matrix(tr_dat[, -1]),
    kernel = "polydot",
    kpar = list(degree = 3, scale = .1)
  )

  pca_pred_exp <- kernlab::predict(pca_exp, te_dat[, -1])[,
    1:kpca_trained$steps[[1]]$num_comp
  ]
  colnames(pca_pred_exp) <- paste0("kPC", 1:kpca_trained$steps[[1]]$num_comp)

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)

  kpca_tibble <-
    tibble(terms = c("X2", "X3", "X4", "X5", "X6"), id = "")

  expect_equal(tidy(kpca_rec, 1), kpca_tibble)
  expect_equal(tidy(kpca_trained, 1), kpca_tibble)
})

test_that("No kPCA comps", {
  pca_extract <- rec |>
    step_kpca_poly(X2, X3, X4, X5, X6, num_comp = 0, id = "") |>
    prep()

  expect_equal(
    names(bake(pca_extract, new_data = NULL)),
    paste0("X", c(2:6, 1))
  )
  expect_null(pca_extract$steps[[1]]$res)
  expect_equal(
    tidy(pca_extract, 1),
    tibble::tibble(terms = paste0("X", 2:6), id = "")
  )
  expect_snapshot(pca_extract)
})

test_that("check_name() is used", {
  skip_if_not_installed("kernlab")
  dat <- dplyr::as_tibble(tr_dat)
  dat$kPC1 <- dat$X1

  rec <- recipe(~., data = dat) |>
    step_kpca_poly(X2, X3, X4, X5, X6)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_kpca_poly(all_predictors())
  rec_param <- tunable.step_kpca_poly(rec$steps[[1]])
  expect_equal(
    rec_param$name,
    c("num_comp", "degree", "scale_factor", "offset")
  )
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 4)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("Do nothing for num_comps = 0 and keep_original_cols = FALSE (#1152)", {
  rec <- recipe(~., data = mtcars) |>
    step_kpca_poly(
      all_predictors(),
      num_comp = 0,
      keep_original_cols = FALSE
    ) |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_identical(res, tibble::as_tibble(mtcars))
})

test_that("rethrows error correctly from implementation", {
  skip_if_not_installed("kernlab")

  local_mocked_bindings(
    .package = "kernlab",
    kpca = function(...) {
      cli::cli_abort("mocked error")
    }
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_kpca_poly(all_predictors()) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("kernlab")

  kpca_rec <- rec |>
    step_kpca_poly(X2, X3, X4, X5, X6, degree = 3, scale_factor = .1) |>
    update_role(X2, X3, X4, X5, X6, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(kpca_trained, new_data = te_dat[, 1:3]))
})

test_that("empty printing", {
  skip_if_not_installed("kernlab")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_kpca_poly(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  skip_if_not_installed("kernlab")

  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_kpca_poly(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  skip_if_not_installed("kernlab")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_kpca_poly(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("kernlab")
  new_names <- paste0("kPC", 1:5)

  rec <- recipe(~., tr_dat) |>
    step_kpca_poly(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~., tr_dat) |>
    step_kpca_poly(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c(colnames(tr_dat), new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("kernlab")
  rec <- recipe(~., tr_dat) |>
    step_kpca_poly(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = tr_dat)
  )
})

test_that("printing", {
  skip_if_not_installed("kernlab")

  kpca_rec <- recipe(X1 ~ ., data = tr_dat) |>
    step_kpca_poly(X2, X3, X4, X5, X6)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_kpca_poly(
      all_predictors(),
      num_comp = hardhat::tune(),
      degree = hardhat::tune(),
      scale_factor = hardhat::tune(),
      offset = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 4L)
})

test_that("bad args", {
  skip_if_not_installed("kernlab")

  expect_snapshot(
    recipe(~., data = tr_dat) |>
      step_kpca_poly(all_numeric_predictors(), num_comp = -1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = tr_dat) |>
      step_kpca_poly(all_numeric_predictors(), degree = 1.1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = tr_dat) |>
      step_kpca_poly(all_numeric_predictors(), scale_factor = -1.1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = tr_dat) |>
      step_kpca_poly(all_numeric_predictors(), offset = "a") |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = tr_dat) |>
      step_kpca_poly(all_numeric_predictors(), prefix = 1) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  skip_if_not_installed("kernlab")

  data <- mtcars
  rec <- recipe(~., data) |>
    step_kpca_poly(all_numeric_predictors()) |>
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
