library(recipes)
library(testthat)

skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")

sacr_tr <- Sacramento[(1:800), ]
sacr_te <- Sacramento[-(1:800), ]

rec <- recipe(~., data = sacr_tr)

test_that("basic functionality", {
  rec_1 <- rec %>%
    step_relevel(zip, ref_level = "z95838") %>%
    prep()

  tr_1 <- juice(rec_1)
  expect_equal(levels(tr_1$zip)[[1]], "z95838")

  te_1 <- bake(rec_1, sacr_te)
  expect_equal(levels(te_1$zip)[[1]], "z95838")
})

test_that("bad args", {
  expect_snapshot(error = TRUE,
    rec %>%
      step_relevel(sqft, ref_level = 23) %>%
      prep()
  )
  expect_snapshot(error = TRUE,
    rec %>%
      step_relevel(city, ref_level = "missing_level") %>%
      prep()
  )
})

test_that("printing", {
  expect_snapshot(print(rec %>% step_relevel(zip, ref_level = "z95838")))
  expect_snapshot(print(rec %>% step_relevel(zip, ref_level = "z95838") %>% prep()))
})


test_that("tidy methods", {
  rec_raw <- rec %>% step_relevel(zip, ref_level = "z95838", id = "city")
  expect_equal(
    tidy(rec_raw, 1),
    tibble(terms = "zip", value = "z95838", id = "city")
  )
  expect_equal(
    tidy(prep(rec_raw), 1),
    tibble(terms = "zip", value = "z95838", id = "city")
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_relevel(rec1, ref_level = "x")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_relevel(rec, ref_level = "x")

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_relevel(rec, ref_level = "x")

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})


test_that("bake method errors when needed non-standard role columns are missing", {
  rec_1 <- rec %>%
    step_relevel(zip, ref_level = "z95838") %>%
    update_role(zip, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE) %>%
    prep()

  expect_error(bake(rec_1, sacr_te[, c(1, 3:ncol(sacr_te))]),
               class = "new_data_missing_column")
})
