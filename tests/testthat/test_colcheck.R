library(testthat)
library(recipes)
library(dplyr)

rp1 <- recipe(mtcars, cyl ~ .)
rp2 <- recipe(mtcars, cyl ~ mpg + drat)

test_that("check_col works in the prep stage", {
  expect_error(rp1 %>% check_cols(everything()) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(everything()) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(cyl, mpg) %>% prep(), NA)
})


test_that("check_col works in the bake stage", {
  expect_error(rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars),
               NA)
  expect_equal(rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars),
               mtcars)
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars), NA)
  expect_equal(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars),
               mtcars[ ,c(1, 2, 5)])
  expect_error(rp1 %>% check_cols(everything()) %>% prep %>% bake(mtcars[-1]),
               "The following cols are missing from newdata: `mpg`.")
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>%
                 bake(mtcars[ ,c(2, 5)]),
               "The following cols are missing from newdata: `mpg`.")
})
