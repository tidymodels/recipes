library(testthat)
library(recipes)
library(modeldata)
library(modeldata)
data(credit_data)

context("Mean imputation")


set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[ in_training, ]
credit_te <- credit_data[-in_training, ]

test_that('simple mean', {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_meanimpute(Age, Assets, Income, id = "")
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  expect_equal(te_imputed$Age, credit_te$Age)

  assets_pred <- mean(credit_tr$Assets, na.rm = TRUE)
  assets_pred <- recipes:::cast(assets_pred, credit_tr$Assets)
  expect_equal(te_imputed$Assets[is.na(credit_te$Assets)],
               rep(assets_pred, sum(is.na(credit_te$Assets))))

  inc_pred <- mean(credit_tr$Income, na.rm = TRUE)
  inc_pred <- recipes:::cast(inc_pred, credit_tr$Assets)
  expect_equal(te_imputed$Income[is.na(credit_te$Income)],
               rep(inc_pred, sum(is.na(credit_te$Income))))

  means <- vapply(credit_tr[, c("Age", "Assets", "Income")],
                 mean, numeric(1), na.rm = TRUE)
  means <- purrr::map2(means, credit_tr[, c("Age", "Assets", "Income")], recipes:::cast)
  means <- unlist(means)

  imp_tibble_un <-
    tibble(terms = c("Age", "Assets", "Income"),
           model = rep(NA_real_, 3),
           id = "")
  imp_tibble_tr <-
    tibble(terms = c("Age", "Assets", "Income"),
           model = means,
           id = "")

  expect_equivalent(as.data.frame(tidy(impute_rec, 1)), as.data.frame(imp_tibble_un))
  expect_equal(tidy(imputed, 1), imp_tibble_tr)

})

test_that('trimmed mean', {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_meanimpute(Assets, trim = .1)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  mean_val <- mean(credit_tr$Assets, na.rm = TRUE, trim = .1)
  mean_val <- recipes:::cast(mean_val, credit_tr$Assets)
  expect_equal(te_imputed$Assets[is.na(credit_te$Assets)],
               rep(mean_val, sum(is.na(credit_te$Assets))))
})

test_that('non-numeric', {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_meanimpute(Assets, Job)
  expect_error(prep(impute_rec, training = credit_tr, verbose = FALSE))
})


test_that('printing', {
  impute_rec <- recipe(Price ~ ., data = credit_tr) %>%
    step_meanimpute(Age, Assets, Income)
  expect_output(print(impute_rec))
  expect_output(prep(impute_rec, training = credit_tr, verbose = TRUE))
})


test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_meanimpute(all_predictors())
  rec_param <- tunable.step_impute_mean(rec$steps[[1]])
  expect_equal(rec_param$name, c("trim"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})
