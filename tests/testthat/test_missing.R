library(testthat)
library(recipes)
library(dplyr)


set_with_na <- tibble(
  a = c(1, 2, NA),
  b = c(1, 2, NA_real_),
  d = as.integer(c(1, 2, NA_integer_)),
  e = c(1, 2, NA_character_)
)

tst <- function(...) {
  cols <- quos(...)
  recipe(set_with_na) %>% check_missing(!!!cols) %>%
    prep() %>% bake(set_with_na)
}

test_that("check_missing passes silently when no NA", {
  no_na_rp <- recipe(mtcars) %>%
    check_missing(all_numeric()) %>%
    prep()
  expect_error(bake(no_na_rp, mtcars), NA)
  expect_equal(bake(no_na_rp, mtcars), tibble(mtcars))
})

test_that("check_missing throws error on all types", {
  expect_error(tst(a),
              "The following columns contain missing values: `a`.")
  expect_error(tst(b),
               "The following columns contain missing values: `b`.")
  expect_error(tst(d),
               "The following columns contain missing values: `d`.")
  expect_error(tst(e),
               "The following columns contain missing values: `e`.")
})

test_that("check_missing works on multiple columns simultaneously" ,{
  expect_error(tst(a, e),
               "The following columns contain missing values: `a`, `e`.")
  expect_error(tst(everything()),
               paste0("The following columns contain missing values: ",
                      "`a`, `b`, `d`, `e`."))
})

test_that("check_missing on a new set", {
  no_na <- tibble(a = 1:3)
  na    <- tibble(a = c(1, NA))
  rp    <- recipe(no_na) %>% check_missing(a) %>% prep(no_na)
  expect_error(bake(rp, na),
               "The following columns contain missing values: `a`.")
})
