library(testthat)
library(recipes)
library(tibble)

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
    step_ratio(x1, denom = denom_vars(all_numeric()))
  rec1 <- prep(rec1, ex_dat, verbose = FALSE)
  obs1 <- bake(rec1, ex_dat)
  res1 <- tibble(
    x1_o_x2   = ex_dat$x1/ex_dat$x2,
    x1_o_x3   = ex_dat$x1/ex_dat$x3,
    x1_o_x4   = ex_dat$x1/ex_dat$x4
  )
  for(i in names(res1)) 
    expect_equal(res1[i], obs1[i])
})


test_that('many:1', {
  rec2 <- rec %>% 
    step_ratio(all_numeric(), denom = denom_vars(x1))
  rec2 <- prep(rec2, ex_dat, verbose = FALSE)
  obs2 <- bake(rec2, ex_dat)
  res2 <- tibble(
    x2_o_x1   = ex_dat$x2/ex_dat$x1,
    x3_o_x1   = ex_dat$x3/ex_dat$x1,
    x4_o_x1   = ex_dat$x4/ex_dat$x1
  )
  for(i in names(res2)) 
    expect_equal(res2[i], obs2[i])
})


test_that('many:many', {
  rec3 <- rec %>% 
    step_ratio(all_numeric(), denom = denom_vars(all_numeric()))
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
  expect_output(prep(rec3, training = ex_dat))
})


