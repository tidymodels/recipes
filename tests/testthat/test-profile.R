library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")
Sacramento <- Sacramento[1:20, ]
Sacramento$city <- factor(Sacramento$city)
Sacramento$int <- 1:20
sacr_rec <- recipe(~., data = Sacramento)

is_unq <- function(x) length(unique(x)) == 1

test_that("numeric profile", {
  num_rec <- sacr_rec %>%
    step_profile(-sqft, profile = vars(sqft)) %>%
    prep(Sacramento) %>%
    bake(new_data = NULL)
  expect_true(is_unq(num_rec$city))
  expect_true(is_unq(num_rec$price))
  expect_true(is_unq(num_rec$zip))
  expect_true(is_unq(num_rec$beds))
  expect_true(is_unq(num_rec$int))
  expect_false(is_unq(num_rec$sqft))

  expect_true(inherits(num_rec$city, "factor"))
  expect_true(inherits(num_rec$price, "integer"))
  expect_true(inherits(num_rec$zip, "factor"))
  expect_true(inherits(num_rec$beds, "integer"))
  expect_true(inherits(num_rec$int, "integer"))
  expect_true(inherits(num_rec$sqft, "integer"))
})


test_that("factor profile", {
  fact_rec <- sacr_rec %>%
    step_profile(-city, profile = vars(city)) %>%
    prep(Sacramento) %>%
    bake(new_data = NULL)
  expect_false(is_unq(fact_rec$city))
  expect_true(is_unq(fact_rec$price))
  expect_true(is_unq(fact_rec$zip))
  expect_true(is_unq(fact_rec$beds))
  expect_true(is_unq(fact_rec$sqft))
})


test_that("beds profile", {
  beds_rec <- sacr_rec %>%
    step_profile(-beds, profile = vars(beds)) %>%
    prep(Sacramento) %>%
    bake(new_data = NULL)
  expect_true(is_unq(beds_rec$city))
  expect_true(is_unq(beds_rec$price))
  expect_true(is_unq(beds_rec$zip))
  expect_false(is_unq(beds_rec$beds))
  expect_true(is_unq(beds_rec$sqft))
})

test_that("character profile", {
  chr_rec <- sacr_rec %>%
    step_profile(-zip, profile = vars(zip)) %>%
    prep(Sacramento, strings_as_factors = FALSE) %>%
    bake(new_data = NULL)
  expect_true(is_unq(chr_rec$city))
  expect_true(is_unq(chr_rec$price))
  expect_false(is_unq(chr_rec$zip))
  expect_true(is_unq(chr_rec$beds))
  expect_true(is_unq(chr_rec$sqft))
})


test_that("bad values", {
  expect_snapshot(error = TRUE,
    sacr_rec %>%
      step_profile(everything(), profile = vars(sqft)) %>%
      prep(data = Sacramento)
  )
  expect_snapshot(error = TRUE,
    sacr_rec %>%
      step_profile(sqft, beds, price, profile = vars(zip, beds)) %>%
      prep(data = Sacramento)
  )
  expect_snapshot(error = TRUE,
    sacr_rec %>%
      step_profile(city, profile = vars(sqft), pct = -1) %>%
      prep(data = Sacramento)
  )
  expect_snapshot(error = TRUE,
    sacr_rec %>%
      step_profile(city, profile = vars(sqft), grid = 1:3) %>%
      prep(data = Sacramento)
  )
  expect_snapshot(error = TRUE,
    sacr_rec %>%
      step_profile(city, profile = vars(sqft), grid = list(pctl = 1, len = 2)) %>%
      prep(data = Sacramento)
  )
  expect_snapshot(error = TRUE,
    fixed(rep(c(TRUE, FALSE), each = 5))
  )
})

test_that("tidy", {
  num_rec_3 <- sacr_rec %>%
    step_profile(-sqft, profile = vars(contains("sqft")), id = "")
  num_rec_4 <- prep(num_rec_3, Sacramento)

  tidy_3 <- tidy(num_rec_3, 1)
  exp_3 <- tibble(
    terms = c("-sqft", "contains(\"sqft\")"),
    type = c("fixed", "profiled"),
    id = ""
  )
  expect_equal(tidy_3, exp_3)

  tidy_4 <- tidy(num_rec_4, 1)
  exp_4 <- tibble(
    terms = c("city", "zip", "beds", "baths", "type", "price", "latitude",
              "longitude", "int", "sqft"),
    type = c(rep("fixed", 9), "profiled"),
    id = ""
  )
  expect_equal(tidy_4, exp_4)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_profile() doesn't work in a way where this is useful
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_profile(rec, profile = vars(mpg))

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_profile(rec1, profile = vars(mpg))

  rec2 <- prep(rec2, mtcars)

  baked2 <- bake(rec2, mtcars)

  expect_named(baked2, "mpg")
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_profile(rec, profile = vars(mpg))

  expect <- tibble(terms = character(), type = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = Sacramento) %>%
    step_profile(-sqft, profile = vars(sqft))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
