library(recipes)
library(testthat)

# ----------------------------------------------------------------

skip_if_not_installed("modeldata")
data("Sacramento", package = "modeldata")

Sacramento_chr <-
  Sacramento |>
  mutate(across(where(is.factor), as.character))

Sacramento_fac <-
  Sacramento |>
  mutate(type = as.character(type))

Sacramento_all_fac <-
  Sacramento

# ----------------------------------------------------------------

test_that("factors all the way down", {
  tr <-
    Sacramento_all_fac |>
    slice(1:500)

  te <-
    Sacramento_all_fac |>
    slice(501:932)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

test_that("factors all the way down with skipping", {
  tr <-
    Sacramento_all_fac |>
    slice(1:500)

  te <-
    Sacramento_all_fac |>
    slice(501:932) |>
    select(-type)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

# ----------------------------------------------------------------

test_that("mixed nominal data", {
  tr <-
    Sacramento_fac |>
    slice(1:500)
  te <-
    Sacramento_fac |>
    slice(501:932)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

test_that("mixed nominal data with skipping", {
  tr <-
    Sacramento_fac |>
    slice(1:500)
  te <-
    Sacramento_fac |>
    slice(501:932) |>
    select(-type)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

# ----------------------------------------------------------------

test_that("no factors", {
  tr <-
    Sacramento_chr |>
    slice(1:500)
  te <-
    Sacramento_chr |>
    slice(501:932)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

test_that("no factors with skipping", {
  tr <-
    Sacramento_chr |>
    slice(1:500)
  te <-
    Sacramento_chr |>
    slice(501:932) |>
    select(-type)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

# ----------------------------------------------------------------

test_that("missing factors", {
  tr <-
    Sacramento_fac |>
    slice(1:500)
  te <-
    Sacramento_chr |>
    slice(501:932)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_snapshot(check_nominal_type(te, rec$orig_lvls))
})

test_that("missing single factor", {
  tr <-
    Sacramento_fac |>
    select(city) |>
    slice(1:500)
  te <-
    Sacramento_chr |>
    select(city) |>
    slice(501:932)

  rec <-
    recipe(~., data = tr) |>
    prep(training = tr)

  expect_snapshot(check_nominal_type(te, rec$orig_lvls))
})

test_that("missing factors with skipping", {
  tr <-
    Sacramento_fac |>
    slice(1:500)
  te <-
    Sacramento_chr |>
    slice(501:932) |>
    select(-type)

  rec <-
    recipe(type ~ ., data = tr) |>
    prep(training = tr)

  expect_snapshot(check_nominal_type(te, rec$orig_lvls))
  expect_snapshot(
    res <- bake(rec, te |> mutate(city = as.character(city)))
  )
})
