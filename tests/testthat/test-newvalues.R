skip_if_not_installed("modeldata")
data("credit_data", package = "modeldata")

x <- rep(letters[1:3], 2)
x_na <- c(rep(letters[1:3], 2), NA)
allowed_values <- letters[1:3]

test_that("new_values_func passes when no new values", {
  expect_no_error(new_values_func(x, allowed_values))
})

test_that("new_values_func breaks when x contains new values", {
  expect_snapshot(
    error = TRUE,
    new_values_func(x, allowed_values[-3], colname = "MacGyver")
  )
})

test_that("new_values_func correctly prints multiple new values", {
  expect_snapshot(
    error = TRUE,
    new_values_func(x, allowed_values[-c(2:3)], colname = "MacGyver")
  )
})

test_that("new_values_func by default ignores NA", {
  expect_no_error(new_values_func(x_na, allowed_values))
})

test_that("new_values_func breaks when NA is new value and ignore_NA is FALSE", {
  expect_snapshot(
    error = TRUE,
    new_values_func(
      x_na,
      allowed_values,
      ignore_NA = FALSE,
      colname = "MacGyver"
    )
  )
})

test_that("new_values_func correctly prints multiple new values with NA", {
  expect_snapshot(
    error = TRUE,
    new_values_func(
      x_na,
      allowed_values[-3],
      ignore_NA = FALSE,
      colname = "MacGyver"
    )
  )
})

test_that("new_values_func correctly prints only non na-values when also NA as new value and ignore_NA is TRUE", {
  expect_snapshot(
    error = TRUE,
    new_values_func(
      x_na,
      allowed_values[-3],
      ignore_NA = TRUE,
      colname = "MacGyver"
    )
  )
})

test_that("check_new_values does nothing when no new values", {
  expect_no_error(
    x <- recipe(credit_data) |>
      check_new_values(Home) |>
      prep() |>
      bake(credit_data)
  )
  expect_equal(x, as_tibble(credit_data))
})

test_that("check_new_values breaks with new values", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:5])

  expect_snapshot(
    error = TRUE,
    recipe(x1) |>
      check_new_values(a) |>
      prep() |>
      bake(x2[1:4, , drop = FALSE])
  )

  expect_snapshot(
    error = TRUE,
    recipe(x1) |> check_new_values(a) |> prep() |> bake(x2)
  )
})

test_that("check_new_values ignores NA by default", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:4] |> c(NA))
  expect_no_error(
    recipe(x1) |>
      check_new_values(a) |>
      prep() |>
      bake(x2[-4, , drop = FALSE])
  )

  expect_snapshot(
    error = TRUE,
    recipe(x1) |> check_new_values(a) |> prep() |> bake(x2)
  )
})

test_that("check_new_values not ignoring NA argument", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:4] |> c(NA))

  expect_snapshot(
    error = TRUE,
    recipe(x1) |>
      check_new_values(a, ignore_NA = FALSE) |>
      prep() |>
      bake(x2[-4, , drop = FALSE])
  )

  expect_snapshot(
    error = TRUE,
    recipe(x1) |>
      check_new_values(a, ignore_NA = FALSE) |>
      prep() |>
      bake(x2)
  )
})

check_new_values_data_type_unit_tests <- function(x1, x2, saf = TRUE) {
  expect_no_error(
    res <- recipe(x1, strings_as_factors = saf) |>
      check_new_values(a) |>
      prep() |>
      bake(x1)
  )

  expect_equal(res, x1)

  expect_snapshot(
    error = TRUE,
    recipe(x1) |> check_new_values(a) |> prep() |> bake(x2)
  )
}

test_that("check_new_values works on doubles", {
  x1 <- tibble(a = c(1.1, 1.2))
  x2 <- tibble(a = c(1.1, 1.2, 1.3))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on integers", {
  x1 <- tibble(a = c(1L, 2L))
  x2 <- tibble(a = c(1L, 2L, 3L))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on factors", {
  x1 <- tibble(a = factor(letters[1:2]))
  x2 <- tibble(a = factor(letters[1:3]))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on characters", {
  x1 <- tibble(a = letters[1:2])
  x2 <- tibble(a = letters[1:3])
  check_new_values_data_type_unit_tests(x1, x2, saf = FALSE)
})

test_that("check_new_values works on logicals", {
  x1 <- tibble(a = c(TRUE, TRUE))
  x2 <- tibble(a = c(TRUE, TRUE, FALSE))
  check_new_values_data_type_unit_tests(x1, x2)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(mtcars) |>
    check_new_values(disp) |>
    update_role(disp, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = mtcars)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = mtcars[, -3]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_new_values(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_new_values(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_new_values(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(mpg ~ ., mtcars) |>
    check_new_values(disp)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(mpg ~ ., mtcars) |>
      check_new_values(disp, ignore_NA = 2) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    check_new_values(all_numeric_predictors()) |>
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
