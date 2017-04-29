library(testthat)
library(magrittr)
library(recipes)
data("credit_data")

set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[ in_training, ]
credit_te <- credit_data[-in_training, ]

test_that('simple mean', {
  rec <- recipe(Price ~ ., data = credit_tr)
  
  impute_rec <- rec %>%
    step_meanimpute(Age, Assets, Income)
  imputed <- learn(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- process(imputed, newdata = credit_te)

  expect_equal(te_imputed$Age, credit_te$Age)
  expect_equal(te_imputed$Assets[is.na(credit_te$Assets)], 
               rep(mean(credit_tr$Assets, na.rm = TRUE), 
                   sum(is.na(credit_te$Assets))))
  expect_equal(te_imputed$Income[is.na(credit_te$Income)], 
               rep(mean(credit_tr$Income, na.rm = TRUE), 
                   sum(is.na(credit_te$Income))))  
})

test_that('trimmed mean', {
  rec <- recipe(Price ~ ., data = credit_tr)
  
  impute_rec <- rec %>%
    step_meanimpute(Assets, trim = .1)
  imputed <- learn(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- process(imputed, newdata = credit_te)
  
  expect_equal(te_imputed$Assets[is.na(credit_te$Assets)], 
               rep(mean(credit_tr$Assets, na.rm = TRUE, trim = .1), 
                   sum(is.na(credit_te$Assets))))
})

test_that('non-numeric', {
  rec <- recipe(Price ~ ., data = credit_tr)
  
  impute_rec <- rec %>%
    step_meanimpute(Assets, Job)
  expect_error(learn(impute_rec, training = credit_tr, verbose = FALSE))
})


