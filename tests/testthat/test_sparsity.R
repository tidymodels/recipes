library(testthat)
library(recipes)
library(Matrix)

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
  bake_default <- bake(rec, newdata = okc_te, all_numeric())
  bake_sparse <-
    bake(rec,
         newdata = okc_te,
         all_numeric(),
         composition = "dgCMatrix")
  bake_sparse_1d <-
    bake(rec,
         newdata = okc_te,
         age,
         composition = "dgCMatrix")
  juice_default <- juice(rec, all_numeric())
  juice_sparse <-
    juice(rec, all_numeric(), composition = "dgCMatrix")
  juice_sparse_1d <-
    juice(rec, age, composition = "dgCMatrix")

  expect_equal(class(bake_default), class(tibble()))
  expect_equal(class(juice_default), class(tibble()))

  expect_equal(as.vector(class(bake_sparse)), "dgCMatrix")
  expect_equal(as.vector(class(juice_sparse)), "dgCMatrix")

  expect_equal(as.vector(class(bake_sparse_1d)), "dgCMatrix")
  expect_equal(as.vector(class(juice_sparse_1d)), "dgCMatrix")

  expect_equal(recipes:::convert_matrix(bake_default),
               bake_sparse)
  expect_equal(recipes:::convert_matrix(juice_default),
               juice_sparse)
})

test_that('bad args', {
  expect_error(bake(rec, newdata = okc_te, composition = "dgCMatrix"))
  expect_error(juice(rec, composition = "dgCMatrix"))
})
