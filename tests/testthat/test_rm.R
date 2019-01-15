library(testthat)
library(recipes)
library(tibble)

context("Removing variables")


n <- 20
set.seed(12)
ex_dat <- data.frame(x1 = rnorm(n),
                     x2 = runif(n))

test_that('basics', {
  rec <- recipe(~., data = ex_dat) %>%
    step_rm(x1)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_rm <- bake(rec_trained, new_data = ex_dat)

  expect_equal(colnames(rec_rm), "x2")
})

# test_that('skipping', {
#   rec <- recipe(~., data = ex_dat) %>%
#     step_rm(x1, skip = TRUE)
#
#   rec_trained <- prep(rec, training = ex_dat, retain = TRUE)
#   tr_res <- juice(rec_trained)
#   te_res <- bake(rec_trained, new_data = ex_dat)
#
#   expect_equal(colnames(tr_res), "x2")
#   expect_equal(colnames(tr_res), c("x1", "x2"))
# })




test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>%
    step_rm(x1)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})

