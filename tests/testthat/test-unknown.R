library(recipes)
library(testthat)
skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")

Sacramento$city <- as.character(Sacramento$city)
Sacramento$zip <- as.character(Sacramento$zip)

sacr_tr <- Sacramento[(1:800), ]
sacr_te <- Sacramento[-(1:800), ]

rec <- recipe(~., data = sacr_tr)

test_that("basic functionality", {
  rec_1 <- rec |>
    step_unknown(city, zip) |>
    prep()

  tr_1 <- bake(rec_1, new_data = NULL)
  tr_city <- tr_1$city[is.na(sacr_tr$city)]
  tr_city <- unique(as.character(tr_city))
  expect_true(all(tr_city == "unknown"))
  city_lvl <- c(sort(unique(sacr_tr$city)), "unknown")
  expect_equal(city_lvl, levels(tr_1$city))

  tr_loc <- tr_1$city[is.na(sacr_tr$zip)]
  tr_loc <- unique(as.character(tr_loc))
  expect_true(all(tr_loc == "unknown"))
  expect_equal(city_lvl, levels(tr_1$city))
  loc_lvl <- c(sort(unique(sacr_tr$zip)), "unknown")
  expect_equal(loc_lvl, levels(tr_1$zip))

  expect_snapshot(
    te_1 <- bake(rec_1, sacr_te)
  )
  te_city <- te_1$city[is.na(sacr_te$city)]
  te_city <- unique(as.character(te_city))
  expect_true(all(te_city == "unknown"))
  expect_equal(city_lvl, levels(te_1$city))

  te_loc <- tr_1$city[is.na(sacr_te$zip)]
  te_loc <- unique(as.character(te_loc))
  expect_true(all(te_loc == "unknown"))
  expect_equal(loc_lvl, levels(te_1$zip))

  rec_2 <- rec |>
    step_unknown(city, new_level = "potato-based") |>
    prep()
  tr_2 <- bake(rec_2, new_data = NULL)
  tr_city <- tr_2$city[is.na(sacr_tr$city)]
  tr_city <- unique(as.character(tr_city))
  expect_true(all(tr_city == "potato-based"))
  city_lvl <- c(sort(unique(sacr_tr$city)), "potato-based")
  expect_equal(city_lvl, levels(tr_2$city))
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = sacr_tr) |>
      step_unknown(sqft) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = sacr_tr) |>
      step_unknown(city, new_level = "FAIR_OAKS") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = sacr_tr) |>
      step_unknown(city, new_level = 2) |>
      prep()
  )
})

test_that("tidy methods", {
  rec_raw <- rec |>
    step_unknown(all_nominal(), new_level = "cake", id = "cheese")

  expect_equal(
    tidy(rec_raw, 1),
    tibble(terms = "all_nominal()", value = "cake", id = "cheese")
  )
  expect_equal(
    tidy(prep(rec_raw), 1),
    tibble(terms = c("city", "zip", "type"), value = "cake", id = "cheese")
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec_1 <- rec |>
    step_unknown(city, zip) |>
    update_role(city, zip, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep()

  expect_snapshot(error = TRUE, bake(rec_1, sacr_te[3:ncol(sacr_te)]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_unknown(rec, new_level = "cake")

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_unknown(rec1, new_level = "cake")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_unknown(rec, new_level = "cake")

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = sacr_tr) |>
    step_unknown(city, zip)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_unknown(Species) |>
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
