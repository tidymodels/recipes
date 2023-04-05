test_that("check_name() is used", {
  skip_if_not_installed("RcppML")
  data("ames", package = "modeldata")
  library(Matrix)
  dat <- mtcars
  dat$NNMF1 <- as.character(dat$mpg)

  rec <- recipe(~ ., data = dat) %>%
    step_nnmf_sparse(all_numeric_predictors())

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})
