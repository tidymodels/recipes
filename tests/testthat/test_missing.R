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
  recipe(set_with_na) %>%
    check_missing(!!!cols) %>%
    prep() %>%
    bake(set_with_na)
}

test_that("check_missing passes silently when no NA", {
  no_na_rp <- recipe(mtcars) %>%
    check_missing(all_numeric()) %>%
    prep()
  expect_error(bake(no_na_rp, mtcars), NA)
  expect_equal(bake(no_na_rp, mtcars), tibble(mtcars))
})

test_that("check_missing throws error on all types", {
  expect_snapshot(error = TRUE, tst(a))
  expect_snapshot(error = TRUE, tst(b))
  expect_snapshot(error = TRUE, tst(d))
  expect_snapshot(error = TRUE, tst(e))
})

test_that("check_missing works on multiple columns simultaneously", {
  expect_snapshot(error = TRUE, tst(a, e))
  expect_snapshot(error = TRUE, tst(everything()))
})

test_that("check_missing on a new set", {
  no_na <- tibble(a = 1:3)
  na <- tibble(a = c(1, NA))
  rp <- recipe(no_na) %>%
    check_missing(a) %>%
    prep(no_na)
  expect_snapshot(error = TRUE,
    bake(rp, na)
  )
})

test_that("printing", {
  rec <- recipe(mtcars) %>%
    check_missing(all_numeric())
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_missing(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_missing(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_missing(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
