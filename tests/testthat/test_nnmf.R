library(testthat)
library(recipes)
library(utils)

R_ver <- as.character(getRversion())

context("NNeg Matrix Fact")
req <- c("dimRed", "NMF")

test_that('Correct values', {
  skip_on_cran()
  skip_if(!(compareVersion(R_ver, "3.6.0") >= 0))
  for (i in req)
    skip_if_not_installed(i)

  for (i in req)
    require(i, character.only = TRUE)

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

  expect_output(rec <- prep(rec, training = iris, verbose = TRUE))

  rec_res <- juice(rec, all_predictors(), composition = "matrix")[1:7,]

  expect_equivalent(rec$steps[[1]]$res@other.data$w, exp_w)

  expect_equal(exp_pred, rec_res)

})


test_that('No NNF', {
  skip_on_cran()
  skip_if(!(compareVersion(R_ver, "3.6.0") >= 0))
  for (i in req)
    skip_if_not_installed(i)

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf(all_predictors(), seed = 2432, num_comp = 0) %>%
    prep()

  expect_equal(
    names(juice(rec)),
    names(iris)
  )
  expect_true(inherits(rec$steps[[1]]$res, "list"))
  expect_output(print(rec),
                regexp = "factorization was not done")
  expect_true(all(is.na(tidy(rec, 1)$value)))
})


test_that('tunable', {
  skip_on_cran()
  skip_if(!(compareVersion(R_ver, "3.6.0") >= 0))
  for (i in req)
    skip_if_not_installed(i)

  rec <-
    recipe(~ ., data = iris) %>%
    step_nnmf(all_predictors())
  rec_param <- tunable.step_nnmf(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp", "num_run"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})

