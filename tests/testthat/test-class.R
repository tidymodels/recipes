library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")

x1 <- rnorm(3)
x2 <- as.POSIXct(1:3, origin = "1970-01-01", tz = "CET")
x <- tibble(x1, x2)
x_newdata <- tibble(x1 = as.character(x1), x2)

x3 <- x2
class(x3) <- c(class(x3), "Julian")
x_newdata_2 <- tibble(x1 = x1, x2 = x3)

test_that("bake_check_class helper function gives expected output", {
  expect_no_error(bake_check_class_core(x1, "numeric", "x1"))
  expect_no_error(bake_check_class_core(x2, c("POSIXct", "POSIXt"), "x1"))
  expect_snapshot(error = TRUE, bake_check_class_core(x1, "character", "x1"))
  expect_snapshot(
    error = TRUE,
    bake_check_class_core(x2, c("POSIXct", "Julian"), "x2")
  )
  expect_no_error(bake_check_class_core(x2, "POSIXct", "x2", TRUE))
  expect_snapshot(error = TRUE, bake_check_class_core(x2, "POSIXct", "x2"))
})

test_that("check_class works when class is learned", {
  rec1 <- recipe(~., x) |>
    check_class(all_predictors()) |>
    prep()

  expect_no_error(bake(rec1, x))
  expect_equal(bake(rec1, x), x)
  expect_snapshot(error = TRUE, bake(rec1, x_newdata))
  expect_snapshot(error = TRUE, bake(rec1, x_newdata_2))
})

test_that("check_class works when class is provided", {
  rec2 <- recipe(x) |>
    check_class(x1, class_nm = "numeric") |>
    prep()

  expect_no_error(bake(rec2, x))
  expect_equal(bake(rec2, x), x)
  expect_snapshot(error = TRUE, bake(rec2, x_newdata))

  rec3 <- recipe(x) |>
    check_class(x2, class_nm = c("POSIXct", "POSIXt")) |>
    prep()

  expect_no_error(bake(rec3, x))
  expect_equal(bake(rec3, x), x)
  expect_snapshot(error = TRUE, bake(rec3, x_newdata_2))

  rec4 <- recipe(x) |>
    check_class(
      x2,
      class_nm = c("POSIXct", "POSIXt"),
      allow_additional = TRUE
    ) |>
    prep()
  expect_no_error(bake(rec4, x_newdata_2))
})

# recipes has internal coercion to character >> factor
test_that("characters are handled correctly", {
  rec5_NULL <- recipe(
    Sacramento[1:10, ],
    sqft ~ .,
    strings_as_factors = FALSE
  ) |>
    check_class(all_predictors()) |>
    prep(Sacramento[1:10, ])

  expect_no_error(bake(rec5_NULL, Sacramento[11:20, ]))

  rec5_man <- recipe(
    Sacramento[1:10, ],
    sqft ~ .,
    strings_as_factors = FALSE
  ) |>
    check_class(city, zip) |>
    prep(Sacramento[1:10, ])

  expect_no_error(bake(rec5_man, Sacramento[11:20, ]))

  sacr_fac <-
    dplyr::mutate(
      Sacramento,
      city = as.character(city),
      zip = as.character(zip),
      type = as.character(type)
    )

  rec6_NULL <- recipe(sacr_fac[1:10, ], sqft ~ ., strings_as_factors = TRUE) |>
    check_class(all_predictors()) |>
    prep(sacr_fac[1:10, ])

  expect_snapshot(error = TRUE, bake(rec6_NULL, sacr_fac[11:20, ]))

  rec6_man <- recipe(sacr_fac[1:10, ], sqft ~ ., strings_as_factors = TRUE) |>
    check_class(type) |>
    prep(sacr_fac[1:10, ])

  expect_snapshot(error = TRUE, bake(rec6_man, sacr_fac[11:20, ]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(x) |>
    check_class(x1, x2) |>
    update_role(x1, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = x[, -1]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_class(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_class(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_class(rec)

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec7 <- recipe(mpg ~ ., mtcars) |>
    check_class(all_predictors())

  expect_snapshot(print(rec7))
  expect_snapshot(prep(rec7))
})

test_that("bad args", {
  expect_snapshot(
    recipe(mpg ~ ., mtcars) |> check_class(all_predictors(), class_nm = 1),
    error = TRUE
  )
  expect_snapshot(
    recipe(mpg ~ ., mtcars) |>
      check_class(all_predictors(), allow_additional = "yes"),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- x
  rec <- recipe(~., data) |>
    check_class(all_predictors()) |>
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
