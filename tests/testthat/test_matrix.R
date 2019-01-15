library(testthat)
library(recipes)

context("Matrix data types")


###################################################################

data(okc)

okc$diet <- as.factor(okc$diet)
okc$date <- as.Date(okc$date)
okc$location <- as.factor(okc$location)

okc_tr <- okc[1:400, ]
okc_te <- okc[(401:800), ]

###################################################################

rec <- recipe( ~ ., data = okc_tr) %>%
  step_modeimpute(all_nominal()) %>%
  step_meanimpute(all_numeric()) %>%
  step_dummy(location, diet) %>%
  prep(training = okc_tr, retain = TRUE)

###################################################################

test_that('correct types', {
  bake_default <- bake(rec, new_data = okc_te, all_numeric())
  bake_sparse <-
    bake(rec,
         new_data = okc_te,
         all_numeric(),
         composition = "matrix")
  bake_sparse_1d <-
    bake(rec,
         new_data = okc_te,
         age,
         composition = "matrix")
  juice_default <- juice(rec, all_numeric())
  juice_sparse <-
    juice(rec, all_numeric(), composition = "matrix")
  juice_sparse_1d <-
    juice(rec, age, composition = "matrix")
  
  expect_equal(class(bake_default), class(tibble()))
  expect_equal(class(juice_default), class(tibble()))
  
  expect_equal(as.vector(class(bake_sparse)), "matrix")
  expect_equal(as.vector(class(juice_sparse)), "matrix")
  
  expect_equal(as.vector(class(bake_sparse_1d)), "matrix")
  expect_equal(as.vector(class(juice_sparse_1d)), "matrix")
  
  expect_equal(recipes:::convert_matrix(bake_default, sparse = FALSE),
               bake_sparse)
  expect_equal(recipes:::convert_matrix(juice_default, sparse = FALSE),
               juice_sparse)
})

test_that('bad args', {
  expect_error(bake(rec, new_data = okc_te, composition = "matrix"))
  expect_error(juice(rec, composition = "matrix"))
})
