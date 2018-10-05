library(testthat)
library(recipes)

context("Integer conversion")


data(okc)

okc$location <- factor(okc$location)

okc_tr <- okc[1:100, ]
okc_tr$age[1] <- NA

okc_te <- okc[101:105, ]
okc_te$age[1] <- NA
okc_te$diet[1] <- "fast food"
okc_te$diet[2] <- NA

check_int <- function(ref, dat) {
  res <- rep(NA_real_, length(dat))
  for (i in seq_along(dat)) {
    if (!is.na(dat[i])) {
      if (dat[i] %in% ref) {
        res[i] <- which(dat[i] == ref)
      } else
        res[i] <- 0
    }
  }
  res
}

test_that('basic functionality', {
  rec <- recipe(Class ~ ., data = okc_tr) %>%
    step_integer(all_predictors())
  rec_trained <- prep(rec, traning = okc_tr, retain = TRUE)
  
  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, okc_te, all_predictors())
  
  ord_dates <- sort(unique(okc_tr$date))
  ord_ages <- sort(unique(okc_tr$age)) 
  ord_diet <- sort(unique(okc_tr$diet[!is.na(okc_tr$diet)]))
  
  expect_equal(te_int$location, as.numeric(okc_te$location))
  expect_equal(te_int$date, check_int(ord_dates, okc_te$date))
  expect_equal(te_int$diet, check_int(ord_diet, okc_te$diet))
  expect_equal(te_int$age, check_int(ord_ages, okc_te$age))
  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(all(!vapply(te_int, is.integer, logical(1))))
  
  expect_equal(tr_int$location, as.numeric(okc_tr$location))
  expect_equal(tr_int$date, check_int(ord_dates, okc_tr$date))
  expect_equal(tr_int$diet, check_int(ord_diet, okc_tr$diet))
  expect_equal(tr_int$age, check_int(ord_ages, okc_tr$age))
  expect_true(all(vapply(tr_int, is.numeric, logical(1))))
  expect_true(all(!vapply(tr_int, is.integer, logical(1))))  
})


test_that('integers', {
  rec <- recipe(Class ~ ., data = okc_tr) %>%
    step_integer(all_predictors(), strict = TRUE)
  rec_trained <- prep(rec, traning = okc_tr, retain = TRUE)
  
  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, okc_te, all_predictors())
  
  expect_true(all(vapply(te_int, is.numeric, logical(1))))
  expect_true(all(vapply(te_int, is.integer, logical(1))))
  expect_true(all(vapply(tr_int, is.numeric, logical(1))))
  expect_true(all(vapply(tr_int, is.integer, logical(1))))   

})

test_that('printing', {
  rec <- recipe(Class ~ ., data = okc_tr)
  ints <- rec %>% step_integer(all_predictors())
  expect_output(print(ints))
  expect_output(prep(ints, training = okc_tr, verbose = TRUE))
})

