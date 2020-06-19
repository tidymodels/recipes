library(testthat)
library(recipes)
library(utils)
library(modeldata)
data(scat)
scat <- na.omit(scat)

context("NNeg Matrix Fact")

## -----------------------------------------------------------------------------

R_ver <- as.character(getRversion())

req <- c("dimRed", "NMF")

test_that('Correct values', {
  skip_on_cran()
  skip_if(!(compareVersion(R_ver, "3.6.0") >= 0))
  for (i in req)
    skip_if_not_installed(i)

  for (i in req)
    require(i, character.only = TRUE)

  # # make test cases
  # dat <- scat[, c("Age", "Length")]
  # factorization <- embed(dat, "NNMF", seed = 2432, nrun = 3)
  # proj_dat <- factorization@apply(dat)
  # nn_proj <- predict(factorization, scat[1:4, c("Age", "Length")])
  # w <- as.vector(factorization@other.data$w)
  exp_w <-
    c(1.56699717680771, 22.7370737796942,
      12.4311739219996, 9.65178637385127)
  exp_pred <-
    tibble::tribble(
      ~NNMF1,             ~NNMF2,
      0.261050107872493,  0.369308260568524,
      0.542310107317385,  0.172968506946406,
      0.309972732260825,  0.202255524654062,
      0.214582632861181,  0.375165664110055,
      0.191348895355525,  0.378094365880821,
      0.237816370366837,  0.372236962339289,
      0.242726669120878, 0.0498462975934273
    )

  rec <- recipe(Species ~ Age + Length, data = scat) %>%
    step_nnmf(all_predictors(), seed = 2432, num_run = 3)
  expect_output(print(rec))

  expect_output(rec <- prep(rec, training = scat, verbose = TRUE))

  rec_res <- juice(rec, all_predictors())[1:7,]

  expect_equivalent(as.vector(rec$steps[[1]]$res@other.data$w), exp_w)

  expect_equivalent(exp_pred, rec_res)

})


test_that('No NNF', {
  skip_on_cran()
  skip_if(!(compareVersion(R_ver, "3.6.0") >= 0))
  for (i in req)
    skip_if_not_installed(i)

  rec <- recipe(Species ~ Age + Length, data = scat) %>%
    step_nnmf(all_predictors(), seed = 2432, num_comp = 0) %>%
    prep()

  expect_equal(
    names(juice(rec)),
    c("Age", "Length", "Species")
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
    recipe(~ Age + Length, data = scat) %>%
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

