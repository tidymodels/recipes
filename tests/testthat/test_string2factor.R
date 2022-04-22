library(recipes)
library(testthat)

n <- 200

set.seed(8575)
ex_dat <- data.frame(
  w = sample(letters[1:3], size = n, replace = TRUE),
  x = sample(LETTERS[1:2], size = n, replace = TRUE),
  y = factor(rep_len(month.abb, n)),
  z = factor(rep_len(month.name, n), ordered = TRUE),
  n = 1:n,
  stringsAsFactors = FALSE
)

rec <- recipe(~., data = ex_dat)

test_that("basic functionality", {
  ex_1 <- rec %>%
    step_string2factor(w, x) %>%
    prep(ex_dat, strings_as_factors = FALSE) %>%
    juice()
  expect_equal(class(ex_1$w), "factor")
  expect_equal(class(ex_1$x), "factor")
  expect_equal(levels(ex_1$w), letters[1:3])
  expect_equal(levels(ex_1$x), LETTERS[1:2])

  ex_2 <- rec %>%
    step_string2factor(w, x, ordered = TRUE) %>%
    prep(ex_dat, strings_as_factors = FALSE) %>%
    juice()
  expect_equal(class(ex_2$w), c("ordered", "factor"))
  expect_equal(class(ex_2$x), c("ordered", "factor"))
  expect_equal(levels(ex_2$w), letters[1:3])
  expect_equal(levels(ex_2$x), LETTERS[1:2])
})

test_that("bad args", {
  expect_snapshot(error = TRUE,
    rec %>%
      step_string2factor(w, n) %>%
      prep(ex_dat)
  )
  expect_snapshot(error = TRUE,
    rec %>%
      step_string2factor(n, ordered = "yes") %>%
      prep(ex_dat)
  )
})


test_that("printing", {
  ex_3 <- rec %>%
    step_string2factor(w, x) %>%
    prep(ex_dat, strings_as_factors = FALSE)
  expect_snapshot(print(ex_3))
  expect_snapshot(prep(ex_3))
})

test_that("pre-made factors", {
  ex_1 <- rec %>%
    step_string2factor(w, x, y, z) %>%
    prep(ex_dat, strings_as_factors = FALSE) %>%
    juice()
  expect_true(inherits(ex_1$w, "factor"))
  expect_true(inherits(ex_1$x, "factor"))
  expect_true(inherits(ex_1$y, "factor"))
  expect_true(inherits(ex_1$z, "factor"))
  expect_true(inherits(ex_1$z, "ordered"))

  expect_equal(ex_1$y, ex_dat$y)
  expect_equal(ex_1$z, ex_dat$z)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_string2factor(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_string2factor(rec)

  expect <- tibble(terms = character(), ordered = logical(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_string2factor(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
