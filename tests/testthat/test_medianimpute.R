library(testthat)
library(recipes)
library(modeldata)
library(modeldata)
data(credit_data)

context("Median imputation")


set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[ in_training, ]
credit_te <- credit_data[-in_training, ]

test_that('simple median', {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_medianimpute(Age, Assets, Income, id = "")
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  expect_equal(te_imputed$Age, credit_te$Age)

  assets_pred <- median(credit_tr$Assets, na.rm = TRUE)
  assets_pred <- recipes:::cast(assets_pred, credit_tr$Assets)
  expect_equal(te_imputed$Assets[is.na(credit_te$Assets)],
               rep(assets_pred, sum(is.na(credit_te$Assets))))

  inc_pred <- median(credit_tr$Income, na.rm = TRUE)
  inc_pred <- recipes:::cast(inc_pred, credit_tr$Assets)
  expect_equal(te_imputed$Income[is.na(credit_te$Income)],
               rep(inc_pred, sum(is.na(credit_te$Income))))

  medians <- vapply(credit_tr[, c("Age", "Assets", "Income")],
                    median, double(1), na.rm = TRUE)
  medians <- purrr::map2(medians, credit_tr[, c("Age", "Assets", "Income")], recipes:::cast)
  medians <- unlist(medians)
  imp_tibble_un <-
    tibble(terms = c("Age", "Assets", "Income"),
           model = rep(NA_real_, 3),
           id = "")
  imp_tibble_tr <-
    tibble(terms = c("Age", "Assets", "Income"),
           model = as.integer(unlist(medians)),
           id = "")

  expect_equivalent(as.data.frame(tidy(impute_rec, 1)), as.data.frame(imp_tibble_un))
  expect_equivalent(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_tr))

})


test_that('non-numeric', {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_medianimpute(Assets, Job)
  expect_error(prep(impute_rec, training = credit_tr, verbose = FALSE))
})


test_that('printing', {
  impute_rec <- recipe(Price ~ ., data = credit_tr) %>%
    step_medianimpute(Age, Assets, Income)
  expect_output(print(impute_rec))
  expect_output(prep(impute_rec, training = credit_tr, verbose = TRUE))
})

