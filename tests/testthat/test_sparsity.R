library(testthat)
library(recipes)
library(Matrix)

context("Spare matrix format")


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
         composition = "dgCMatrix")
  bake_sparse_1d <-
    bake(rec,
         new_data = okc_te,
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
  expect_error(bake(rec, new_data = okc_te, composition = "dgCMatrix"))
  expect_error(juice(rec, composition = "dgCMatrix"))
})

test_that('issue 206 - NA values are kept when requesting matrix composition', {
  df <- data.frame(x = factor(c("x", "x", "y")), x2 = c(NA, 1, NA))

  rec <- recipe(x2 ~ ., data = df) %>%
    step_dummy(x) %>%
    prep(df)

  res_mat <- bake(rec, df, composition = "matrix")
  res_sparse <- bake(rec, df, composition = "dgCMatrix")

  expect_equal(nrow(res_mat), 3)
  expect_equal(nrow(res_sparse), 3)

  expect_equal(as.vector(res_mat[,"x2"]), df$x2)
  expect_equal(as.vector(res_sparse[,"x2"]), df$x2)

})


test_that('data.frame_of_matrix format', {

  # data.frame method
  n <- 100
  n_lag = 20
  df <- data.frame(x = runif(n),
                   y = rnorm(n))

  # test with juice
  role_name <- 'lag_variable'
  rec <- recipe(y~., data = df) %>%
    step_lag(x, lag = 1:n_lag, role = role_name) %>%
    prep() %>%
    juice(composition = 'data.frame_of_matrix')

  expect_equal(ncol(rec), 3)
  expect_equal(nrow(rec), n)
  expect_equal(sort(names(rec)), sort(c(role_name, 'outcome', 'predictor')))
  expect_equal(unique(sapply(rec, class)), 'AsIs')
  expect_equivalent(sort(sapply(rec, ncol)), sort(c(1,1,n_lag)))

  # check for bloat
  rec_size <- object.size(rec)
  opt_size <- object.size(df$x) * (n_lag + 2) + object.size(tibble()) * 3
  expect_true(as.numeric(opt_size) / as.numeric(rec_size) > 0.95)

  # test with bake
  role_name <- 'lag_variable'
  rec <- recipe(y~., data = df) %>%
    step_lag(x, lag = 0:2, role = role_name) %>%
    prep() %>%
    bake(df, composition = 'data.frame_of_matrix')

  expect_equal(ncol(rec), 3)
  expect_equal(nrow(rec), n)
  expect_equal(sort(names(rec)), sort(c(role_name, 'outcome', 'predictor')))
  expect_equal(unique(sapply(rec, class)), 'AsIs')

})


test_that('tibble_of_matrix format', {

  n <- 100
  n_lag = 10
  df <- tibble(x = runif(n),
               y = rnorm(n))

  # test with juice
  role_name <- 'lag_variable'
  rec <- recipe(y~., data = df) %>%
    step_lag(x, lag = 1:n_lag, role = role_name) %>%
    prep() %>%
    juice(composition = 'tibble_of_matrix')

  expect_equal(ncol(rec), 3)
  expect_equal(nrow(rec), n)
  expect_equal(sort(names(rec)), sort(c(role_name, 'outcome', 'predictor')))
  expect_equal(unique(sapply(rec, class)), 'matrix')
  expect_equivalent(sort(sapply(rec, ncol)), sort(c(1,1,n_lag)))

  # check for bloat
  rec_size <- object.size(rec)
  opt_size <- object.size(df$x) * (n_lag + 2) + object.size(tibble()) * 3
  expect_true(as.numeric(opt_size) / as.numeric(rec_size) > 0.95)


  # test with bake
  role_name <- 'lag_variable'
  rec <- recipe(y~., data = df) %>%
    step_lag(x, lag = 0:2, role = role_name) %>%
    prep() %>%
    bake(df, composition = 'tibble_of_matrix')

  expect_equal(ncol(rec), 3)
  expect_equal(nrow(rec), n)
  expect_equal(sort(names(rec)), sort(c(role_name, 'outcome', 'predictor')))
  expect_equal(unique(sapply(rec, class)), 'matrix')



  # test with juice
  n <- 10
  df <- tibble(y = rnorm(n),
               x = factor(letters[rep(1:5, 2)]))

  rec <- recipe(y~., data = df) %>%
    step_dummy(x, role = 'dummy') %>%
    prep() %>%
    juice(composition = 'tibble_of_matrix')

  expect_equal(unique(sapply(rec, class)), 'matrix')
  expect_equal(ncol(rec), 2)
  expect_equal(nrow(rec), n)


  # test with bake
  rec <- recipe(y~., data = df) %>%
    step_dummy(x, role = 'dummy') %>%
    prep() %>%
    bake(df, composition = 'tibble_of_matrix')

  expect_equal(unique(sapply(rec, class)), 'matrix')
  expect_equal(ncol(rec), 2)
  expect_equal(nrow(rec), n)

})

test_that('bad convert_matrix args', {
  dftib <- tibble(x = as.character(1:10),
                  y = as.character(1:10))
  expect_error(convert_matrix(dftib),
               'Columns (`x`, `y`) are not numeric; cannot convert to matrix.',
               fixed=TRUE)
  dftib <- tibble(a = as.character(1:10),
                  b = as.character(1:10),
                  c = as.character(1:10),
                  d = as.character(1:10),
                  e = as.character(1:10))
  expect_error(convert_matrix(dftib),
               '5 columns are not numeric; cannot convert to matrix.',
               fixed=TRUE)
})


