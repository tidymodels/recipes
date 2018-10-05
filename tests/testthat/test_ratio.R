library(testthat)
library(recipes)
library(tibble)

context("Ratio creation")


n <- 20
ex_dat <- data.frame(
  x1 = -1:8,
  x2 = 1,
  x3 = c(1:9, NA),
  x4 = 11:20,
  x5 = letters[1:10]
)

rec <- recipe( ~ x1 + x2 + x3 + x4 + x5, data = ex_dat)

test_that('1:many', {
  rec1 <- rec %>%
    step_ratio(x1, denom = denom_vars(all_numeric()), id = "")

  exp_un_1 <- tibble(
    terms = "x1", denom = "all_numeric()", id = ""
  )
  expect_equal(tidy(rec1, number = 1), exp_un_1)

  rec1 <- prep(rec1, ex_dat, verbose = FALSE)
  obs1 <- bake(rec1, ex_dat)
  res1 <- tibble(
    x1_o_x2   = ex_dat$x1/ex_dat$x2,
    x1_o_x3   = ex_dat$x1/ex_dat$x3,
    x1_o_x4   = ex_dat$x1/ex_dat$x4
  )
  for(i in names(res1))
    expect_equal(res1[i], obs1[i])

  exp_tr_1 <- tibble(
    terms = rep("x1", 3),
    denom = c("x2", "x3", "x4"),
    id = ""
  )
  expect_equal(tidy(rec1, number = 1), exp_tr_1)
})


test_that('many:1', {
  rec2 <- rec %>%
    step_ratio(all_numeric(), denom = denom_vars(x1), id = "")

  exp_un_2 <- tibble(
    terms = "all_numeric()", denom = "x1", id = ""
  )
  expect_equal(tidy(rec2, number = 1), exp_un_2)


  rec2 <- prep(rec2, ex_dat, verbose = FALSE)
  obs2 <- bake(rec2, ex_dat)
  res2 <- tibble(
    x2_o_x1   = ex_dat$x2/ex_dat$x1,
    x3_o_x1   = ex_dat$x3/ex_dat$x1,
    x4_o_x1   = ex_dat$x4/ex_dat$x1
  )
  for(i in names(res2))
    expect_equal(res2[i], obs2[i])

  exp_tr_2 <- tibble(
    terms = c("x2", "x3", "x4"),
    denom = rep("x1", 3),
    id = ""
  )
  expect_equal(tidy(rec2, number = 1), exp_tr_2)
})


test_that('many:many', {
  rec3 <- rec %>%
    step_ratio(all_numeric(), denom = denom_vars(all_numeric()), id = "")

  exp_un_3 <- tibble(
    terms = "all_numeric()", denom = "all_numeric()", id = ""
  )
  expect_equal(tidy(rec3, number = 1), exp_un_3)

  rec3 <- prep(rec3, ex_dat, verbose = FALSE)
  obs3 <- bake(rec3, ex_dat)
  res3 <- tibble(
    x2_o_x1   = ex_dat$x2/ex_dat$x1,
    x3_o_x1   = ex_dat$x3/ex_dat$x1,
    x4_o_x1   = ex_dat$x4/ex_dat$x1,

    x1_o_x2   = ex_dat$x1/ex_dat$x2,
    x3_o_x2   = ex_dat$x3/ex_dat$x2,
    x4_o_x2   = ex_dat$x4/ex_dat$x2,

    x1_o_x3   = ex_dat$x1/ex_dat$x3,
    x2_o_x3   = ex_dat$x2/ex_dat$x3,
    x4_o_x3   = ex_dat$x4/ex_dat$x3,

    x1_o_x4   = ex_dat$x1/ex_dat$x4,
    x2_o_x4   = ex_dat$x2/ex_dat$x4,
    x3_o_x4   = ex_dat$x3/ex_dat$x4
  )
  for(i in names(res3))
    expect_equal(res3[i], obs3[i])

  exp_tr_3 <- expand.grid(
    terms = paste0("x", 1:4),
    denom = paste0("x", 1:4),
    stringsAsFactors = FALSE
  )
  exp_tr_3 <- exp_tr_3[exp_tr_3$terms != exp_tr_3$denom,]
  exp_tr_3$id <- ""
  exp_tr_3 <- as_tibble(exp_tr_3)

  expect_equal(tidy(rec3, number = 1), exp_tr_3)
})



test_that('wrong type', {
  rec4 <- rec %>%
    step_ratio(x1, denom = denom_vars(all_predictors()))
  expect_error(prep(rec4, ex_dat, verbose = FALSE))

  rec5 <- rec %>%
    step_ratio(all_predictors(), denom = denom_vars(x1))
  expect_error(prep(rec5, ex_dat, verbose = FALSE))

  rec6 <- rec %>%
    step_ratio(all_predictors(), denom = denom_vars(all_predictors()))
  expect_error(prep(rec6, ex_dat, verbose = FALSE))
})


test_that('printing', {
  rec3 <- rec %>%
    step_ratio(all_numeric(), denom = denom_vars(all_numeric()))
  expect_output(print(rec3))
  expect_output(prep(rec3, training = ex_dat, verbose = TRUE))
})


