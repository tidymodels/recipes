library(testthat)
library(recipes)
skip_if_not_installed("modeldata")
data(credit_data, package = "modeldata")

set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[in_training, ]
credit_te <- credit_data[-in_training, ]

test_that("simple mean", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mean(Age, Assets, Income, id = "")
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  expect_equal(te_imputed$Age, credit_te$Age)

  assets_pred <- mean(credit_tr$Assets, na.rm = TRUE)
  assets_pred <- recipes:::cast(assets_pred, credit_tr$Assets)
  expect_equal(
    te_imputed$Assets[is.na(credit_te$Assets)],
    rep(assets_pred, sum(is.na(credit_te$Assets)))
  )

  inc_pred <- mean(credit_tr$Income, na.rm = TRUE)
  inc_pred <- recipes:::cast(inc_pred, credit_tr$Assets)
  expect_equal(
    te_imputed$Income[is.na(credit_te$Income)],
    rep(inc_pred, sum(is.na(credit_te$Income)))
  )

  means <- vapply(
    credit_tr[, c("Age", "Assets", "Income")],
    mean,
    numeric(1),
    na.rm = TRUE
  )
  means <- purrr::map2(
    means,
    credit_tr[, c("Age", "Assets", "Income")],
    recipes:::cast
  )
  means <- unlist(means)

  imp_tibble_un <-
    tibble(
      terms = c("Age", "Assets", "Income"),
      value = rep(NA_real_, 3),
      id = ""
    )
  imp_tibble_tr <-
    tibble(
      terms = c("Age", "Assets", "Income"),
      value = unname(means),
      id = ""
    )

  expect_equal(as.data.frame(tidy(impute_rec, 1)), as.data.frame(imp_tibble_un))
  expect_equal(tidy(imputed, 1), imp_tibble_tr)
})

test_that("trimmed mean", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mean(Assets, trim = .1)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  mean_val <- mean(credit_tr$Assets, na.rm = TRUE, trim = .1)
  mean_val <- recipes:::cast(mean_val, credit_tr$Assets)
  expect_equal(
    te_imputed$Assets[is.na(credit_te$Assets)],
    rep(mean_val, sum(is.na(credit_te$Assets)))
  )
})

test_that("non-numeric", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mean(Assets, Job)
  expect_snapshot(
    error = TRUE,
    prep(impute_rec, training = credit_tr, verbose = FALSE)
  )
})

test_that("all NA values", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mean(Age, Assets)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  imputed_te <- bake(imputed, new_data = credit_te |> mutate(Age = NA))

  expect_equal(unique(imputed_te$Age), imputed$steps[[1]]$means$Age)
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_impute_mean(all_predictors())
  rec_param <- tunable.step_impute_mean(rec$steps[[1]])
  expect_equal(rec_param$name, c("trim"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("trim works", {
  set.seed(1234)
  x <- rnorm(1000)
  x[sample(seq_along(x), 100)] <- NA

  expect_equal(
    purrr::map(seq(0, 1, by = 0.1), \(.x) mean(x, trim = .x, na.rm = TRUE)),
    purrr::map(
      seq(0, 1, by = 0.1),
      \(.x) trim(x, trim = .x) |> mean(na.rm = TRUE)
    )
  )
})

test_that("case weights", {
  credit_tr_cw <- credit_tr |>
    mutate(Amount = frequency_weights(Amount))

  impute_rec <- recipe(Price ~ ., data = credit_tr_cw) |>
    step_impute_mean(Age, Assets, Income) |>
    prep()

  ref_means <- credit_tr |>
    select(Age, Assets, Income) |>
    averages(wts = credit_tr_cw$Amount) |>
    purrr::map(round, 0)

  expect_equal(
    impute_rec$steps[[1]]$means,
    ref_means
  )

  # Trimmed
  impute_rec <- recipe(Price ~ ., data = credit_tr_cw) |>
    step_impute_mean(Age, Assets, Income, trim = 0.2) |>
    prep()

  ref_means <- credit_tr |>
    dplyr::select(Age, Assets, Income) |>
    purrr::map(trim, trim = 0.2) |>
    purrr::map(
      weighted.mean,
      w = as.numeric(credit_tr_cw$Amount),
      na.rm = TRUE
    ) |>
    purrr::map(round, 0)

  expect_equal(
    impute_rec$steps[[1]]$means,
    ref_means
  )

  expect_snapshot(impute_rec)

  # ----------------------------------------------------------------------------

  credit_tr_cw <- credit_tr |>
    mutate(Amount = importance_weights(Amount))

  impute_rec <- recipe(Price ~ ., data = credit_tr_cw) |>
    step_impute_mean(Age, Assets, Income) |>
    prep()

  ref_means <- credit_tr |>
    select(Age, Assets, Income) |>
    averages(wts = NULL) |>
    purrr::map(round, 0)

  expect_equal(
    impute_rec$steps[[1]]$means,
    ref_means
  )

  # Trimmed
  impute_rec <- recipe(Price ~ ., data = credit_tr_cw) |>
    step_impute_mean(Age, Assets, Income, trim = 0.2) |>
    prep()

  ref_means <- credit_tr |>
    dplyr::select(Age, Assets, Income) |>
    purrr::map(trim, trim = 0.2) |>
    purrr::map(\(x) weighted.mean(x, w = rep(1, length(x)), na.rm = TRUE)) |>
    purrr::map(round, 0)

  expect_equal(
    impute_rec$steps[[1]]$means,
    ref_means
  )

  expect_snapshot(impute_rec)
})

test_that("doesn't destroy sparsity", {
  credit_tr$Debt <- sparsevctrs::as_sparse_double(credit_tr$Debt)
  rec <- recipe(~Debt, data = credit_tr) |>
    step_impute_mean(Debt)

  rec_trained <- prep(rec, training = credit_tr, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = credit_tr)

  expect_true(all(vapply(rec_trans, sparsevctrs::is_sparse_double, logical(1))))

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mean(Age) |>
    update_role(Age, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(imputed, new_data = credit_te[, c(-5)]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_mean(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_impute_mean(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_mean(rec)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(Price ~ ., data = credit_tr) |>
    step_impute_mean(Age, Assets, Income)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_impute_mean(
      all_predictors(),
      trim = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_impute_mean(
        all_predictors(),
        trim = 0.6
      ) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_impute_mean(disp, mpg) |>
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
