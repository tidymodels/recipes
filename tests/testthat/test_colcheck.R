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
               tibble(mtcars[ ,c(1, 3:11, 2)]))
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars), NA)
  expect_equal(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars),
               tibble(mtcars[ ,c(1, 5, 2)]))
  expect_error(rp1 %>% check_cols(everything()) %>% prep %>% bake(mtcars[-1]),
               "The following cols are missing from `new_data`: `mpg`.")
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>%
                 bake(mtcars[ ,c(2, 5)]),
               "The following cols are missing from `new_data`: `mpg`.")
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_cols(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_cols(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_cols(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
