library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(covers, package = "modeldata")
rec <- recipe(~description, covers) |>
  step_regex(description, pattern = "(rock|stony)", result = "rocks") |>
  step_regex(description, pattern = "(rock|stony)", result = "more_rocks")

test_that("default options", {
  rec1 <- rec |> step_bin2factor(rocks)
  rec1 <- prep(rec1, training = covers)
  res1 <- bake(rec1, new_data = covers)
  expect_true(all(diag(table(res1$rocks, res1$more_rocks)) == 0))
})

test_that("works with logicals", {
  mtcars$vs <- as.logical(mtcars$vs)
  mtcars$am <- as.logical(mtcars$am)

  res <- recipe(~., data = mtcars) |>
    step_bin2factor(all_logical_predictors()) |>
    prep() |>
    bake(new_data = NULL)

  expect_identical(
    factor(mtcars$vs, levels = c(TRUE, FALSE), labels = c("yes", "no")),
    res$vs
  )
  expect_identical(
    factor(mtcars$am, levels = c(TRUE, FALSE), labels = c("yes", "no")),
    res$am
  )
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_bin2factor(all_logical_predictors(), ref_first = 1),
    error = TRUE
  )
})

test_that("nondefault options", {
  rec2 <- rec |> step_bin2factor(rocks, levels = letters[2:1])
  rec2 <- prep(rec2, training = covers)
  res2 <- bake(rec2, new_data = covers)
  expect_true(all(diag(table(res2$rocks, res2$more_rocks)) == 0))
})

test_that("bad options", {
  rec3 <- rec |> step_bin2factor(description)
  expect_snapshot(error = TRUE, prep(rec3, training = covers))
  expect_snapshot(
    error = TRUE,
    rec |> step_bin2factor(rocks, levels = letters[1:5])
  )
  expect_snapshot(error = TRUE, rec |> step_bin2factor(rocks, levels = 1:2))
})

test_that("choose reference level", {
  rec4 <- rec |> step_bin2factor(rocks, ref_first = FALSE)
  rec4 <- prep(rec4, training = covers)
  res4 <- bake(rec4, new_data = covers)
  expect_true(all(res4$rocks[res4$more_rocks == 1] == "yes"))
  expect_true(all(res4$rocks[res4$more_rocks == 0] == "no"))
  expect_true(levels(res4$rocks)[1] == "no")
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  mtcars_bin <- mtcars |> mutate(bin = c(1, rep(0, nrow(mtcars) - 1)))

  rec <- recipe(mpg ~ ., mtcars_bin)
  rec <- step_bin2factor(rec, "bin") |>
    update_role(bin, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec <- prep(rec, mtcars_bin)

  expect_snapshot(error = TRUE, bake(rec, mtcars))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_bin2factor(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_bin2factor(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_bin2factor(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec2 <- rec |>
    step_bin2factor(rocks, levels = letters[2:1])

  expect_snapshot(print(rec2))
  expect_snapshot(prep(rec2))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_bin2factor(am, vs) |>
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
