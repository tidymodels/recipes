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
  ex_1 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_string2factor(w, x) |>
    prep(ex_dat) |>
    bake(new_data = NULL)
  expect_equal(class(ex_1$w), "factor")
  expect_equal(class(ex_1$x), "factor")
  expect_equal(levels(ex_1$w), letters[1:3])
  expect_equal(levels(ex_1$x), LETTERS[1:2])

  ex_2 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_string2factor(w, x, ordered = TRUE) |>
    prep(ex_dat) |>
    bake(new_data = NULL)
  expect_equal(class(ex_2$w), c("ordered", "factor"))
  expect_equal(class(ex_2$x), c("ordered", "factor"))
  expect_equal(levels(ex_2$w), letters[1:3])
  expect_equal(levels(ex_2$x), LETTERS[1:2])
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    rec |>
      step_string2factor(w, n) |>
      prep(ex_dat)
  )
  expect_snapshot(
    error = TRUE,
    rec |>
      step_string2factor(w, x, ordered = "yes") |>
      prep(ex_dat)
  )
})

test_that("pre-made factors", {
  ex_1 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_string2factor(w, x, y, z) |>
    prep(ex_dat) |>
    bake(new_data = NULL)
  expect_true(inherits(ex_1$w, "factor"))
  expect_true(inherits(ex_1$x, "factor"))
  expect_true(inherits(ex_1$y, "factor"))
  expect_true(inherits(ex_1$z, "factor"))
  expect_true(inherits(ex_1$z, "ordered"))

  expect_equal(ex_1$y, ex_dat$y)
  expect_equal(ex_1$z, ex_dat$z)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- rec |>
    step_string2factor(w, x) |>
    update_role(w, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = ex_dat[, -1]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_string2factor(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_string2factor(w, x)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  data$vs <- as.character(data$vs)
  rec <- recipe(~., data) |>
    step_string2factor(vs) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
