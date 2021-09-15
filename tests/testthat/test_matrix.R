library(testthat)
library(recipes)


###################################################################

library(modeldata)
data(okc)

okc$diet <- as.factor(okc$diet)
okc$date <- as.Date(okc$date)
okc$location <- as.factor(okc$location)

okc_tr <- okc[1:400, ]
okc_te <- okc[(401:800), ]

###################################################################

rec <- recipe( ~ ., data = okc_tr) %>%
  step_impute_mode(all_nominal()) %>%
  step_impute_mean(all_numeric()) %>%
  step_dummy(location, diet) %>%
  prep(training = okc_tr)

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

  expect_true(inherits(bake_sparse, "matrix"))
  expect_true(inherits(juice_sparse, "matrix"))

  expect_true(inherits(bake_sparse_1d, "matrix"))
  expect_true(inherits(juice_sparse_1d, "matrix"))

  expect_equal(recipes:::convert_matrix(bake_default, sparse = FALSE),
               bake_sparse)
  expect_equal(recipes:::convert_matrix(juice_default, sparse = FALSE),
               juice_sparse)
})

test_that('bad args', {
  expect_error(bake(rec, new_data = okc_te, composition = "matrix"))
  expect_error(juice(rec, composition = "matrix"))
})
