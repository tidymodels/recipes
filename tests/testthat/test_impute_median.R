library(testthat)
library(recipes)
skip_if_not_installed("modeldata")
data(credit_data, package = "modeldata")


set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[in_training, ]
credit_te <- credit_data[-in_training, ]

test_that("simple median", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_impute_median(Age, Assets, Income, id = "")
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  expect_equal(te_imputed$Age, credit_te$Age)

  assets_pred <- median(credit_tr$Assets, na.rm = TRUE)
  assets_pred <- recipes:::cast(assets_pred, credit_tr$Assets)
  expect_equal(
    te_imputed$Assets[is.na(credit_te$Assets)],
    rep(assets_pred, sum(is.na(credit_te$Assets)))
  )

  inc_pred <- median(credit_tr$Income, na.rm = TRUE)
  inc_pred <- recipes:::cast(inc_pred, credit_tr$Assets)
  expect_equal(
    te_imputed$Income[is.na(credit_te$Income)],
    rep(inc_pred, sum(is.na(credit_te$Income)))
  )

  medians <- vapply(credit_tr[, c("Age", "Assets", "Income")],
    median, double(1),
    na.rm = TRUE
  )
  medians <- purrr::map2(medians, credit_tr[, c("Age", "Assets", "Income")], recipes:::cast)
  medians <- unlist(medians)
  imp_tibble_un <-
    tibble(
      terms = c("Age", "Assets", "Income"),
      model = rep(NA_real_, 3),
      id = ""
    )
  imp_tibble_tr <-
    tibble(
      terms = c("Age", "Assets", "Income"),
      model = as.integer(unlist(medians)),
      id = ""
    )

  expect_equal(as.data.frame(tidy(impute_rec, 1)), as.data.frame(imp_tibble_un))
  expect_equal(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_tr))
})


test_that("non-numeric", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_impute_median(Assets, Job)
  expect_snapshot(error = TRUE,
    prep(impute_rec, training = credit_tr, verbose = FALSE)
  )
})

test_that("all NA values", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_impute_median(Age, Assets)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  imputed_te <- bake(imputed, new_data = credit_te %>% mutate(Age = NA))

  expect_equal(unique(imputed_te$Age), imputed$steps[[1]]$medians$Age)
})

test_that("Deprecation warning", {
  expect_snapshot(error = TRUE,
    recipe(~ ., data = mtcars) %>%
      step_medianimpute()
  )
})

test_that("printing", {
  impute_rec <- recipe(Price ~ ., data = credit_tr) %>%
    step_impute_median(Age, Assets, Income)
  expect_snapshot(print(impute_rec))
  expect_snapshot(prep(impute_rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_impute_median(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_median(rec)

  expect <- tibble(terms = character(), model = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_median(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("case weights", {
  credit_tr_cw <- credit_tr %>%
    mutate(Amount = frequency_weights(Amount))

  impute_rec <- recipe(Price ~ ., data = credit_tr_cw) %>%
    step_impute_median(Age, Assets, Income, id = "") %>%
    prep()

  ref_medians <- credit_tr_cw %>%
    select(Age, Assets, Income) %>%
    medians(credit_tr_cw$Amount)

  expect_equal(
    impute_rec$steps[[1]]$medians,
    as.list(ref_medians)
  )

  expect_snapshot(impute_rec)

  # ----------------------------------------------------------------------------

  credit_tr_cw <- credit_tr %>%
    mutate(Amount = importance_weights(Amount))

  impute_rec <- recipe(Price ~ ., data = credit_tr_cw) %>%
    step_impute_median(Age, Assets, Income, id = "") %>%
    prep()

  ref_medians <- credit_tr_cw %>%
    select(Age, Assets, Income) %>%
    medians(wts = NULL)

  expect_equal(
    impute_rec$steps[[1]]$medians,
    as.list(ref_medians)
  )

  expect_snapshot(impute_rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_impute_median(Age) %>%
    update_role(Age, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)

  expect_error(bake(imputed, new_data = credit_te[, c(-5)]),
               class = "new_data_missing_column")
})
