library(testthat)
library(recipes)

context("NNeg Matrix Fact")

test_that('Correct values', {
  skip_on_cran()
  skip_if_not_installed("dimRed")
  skip_if_not_installed("NMF")
  
  library(dimRed)
  library(NMF)

  # # make test cases
  # dat <- loadDataSet("Iris")
  # factorization <- embed(dat, "NNMF", seed = 2432, nrun = 3)
  # proj_dat <- factorization@apply(dat)
  # nn_proj <- predict(factorization, iris[1:7, 1:4])
  exp_w <-
    structure(
      c(
        6.8773292872624,
        4.97298823171793,
        1.31813854010485,
        0.0134584286822877,
        8.65607280130039,
        3.20684079829485,
        8.55325512611619,
        3.12136454264361
      ),
      .Dim = c(4L, 2L),
      .Dimnames = list(
        c("Sepal.Length",
          "Sepal.Width", "Petal.Length", "Petal.Width"),
        c("NNMF1", "NNMF2")
      )
    )
  exp_pred <-
    structure(
      c(
        0.664359302949723,
        0.590439874293881,
        0.607417647961871,
        0.566245763793409,
        0.667498802933088,
        0.692004564217231,
        0.605796119468035,
        0.0613037368385686,
        0.0796317857686579,
        0.0592956942623502,
        0.0836713814327281,
        0.0564414281690253,
        0.0890848181660404,
        0.0657435355504658
      ),
      .Dim = c(7L,
               2L),
      .Dimnames = list(NULL,
                       c("NNMF1", "NNMF2"))
    )

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf(all_predictors(), seed = 2432, num_run = 3)
  expect_output(print(rec))

  expect_output(rec <- prep(rec, training = iris, verbose = TRUE, retain = TRUE))

  rec_res <- juice(rec, all_predictors(), composition = "matrix")[1:7,]

  expect_equivalent(rec$steps[[1]]$res@other.data$w, exp_w)

  expect_equal(exp_pred, rec_res)

})
