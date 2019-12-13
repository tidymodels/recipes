library(testthat)
library(recipes)
library(modeldata)
library(modeldata)
data(credit_data)

context("Mode imputation")


set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[ in_training, ]
credit_te <- credit_data[-in_training, ]

test_that('simple modes', {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_modeimpute(Status, Home, Marital, id = "")
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  expect_equal(te_imputed$Status, credit_te$Status)
  home_exp <- rep(recipes:::mode_est(credit_tr$Home),
                  sum(is.na(credit_te$Home)))
  home_exp <- factor(home_exp, levels = levels(credit_te$Home))
  expect_equal(te_imputed$Home[is.na(credit_te$Home)],
               home_exp)
  marital_exp <- rep(recipes:::mode_est(credit_tr$Marital),
                  sum(is.na(credit_te$Marital)))
  marital_exp <- factor(marital_exp, levels = levels(credit_te$Marital))
  expect_equal(te_imputed$Marital[is.na(credit_te$Marital)],
               marital_exp)

  modes <- vapply(credit_tr[, c("Status", "Home", "Marital")],
                  recipes:::mode_est, character(1))
  imp_tibble_un <-
    tibble(terms = c("Status", "Home", "Marital"),
           model = rep(NA_character_, 3),
           id = "")
  imp_tibble_tr <-
    tibble(terms = c("Status", "Home", "Marital"),
           model = modes,
           id = "")

  expect_equivalent(as.data.frame(tidy(impute_rec, 1)), as.data.frame(imp_tibble_un))
  expect_equivalent(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_tr))

})


test_that('non-nominal', {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec %>%
    step_modeimpute(Assets, Job)
  expect_error(prep(impute_rec, training = credit_tr, verbose = FALSE))
})

test_that('printing', {
  impute_rec <- recipe(Price ~ ., data = credit_tr) %>%
    step_modeimpute(Status, Home, Marital)
  expect_output(print(impute_rec))
  expect_output(prep(impute_rec, training = credit_tr, verbose = TRUE))
})
