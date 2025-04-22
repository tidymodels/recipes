library(testthat)
library(recipes)

lmh <- c("Low", "Med", "High")

examples <- data.frame(
  X1 = factor(rep(letters[1:4], each = 3)),
  X2 = ordered(rep(lmh, each = 4), levels = lmh)
)
rec <- recipe(~ X1 + X2, data = examples)

test_that("correct var", {
  rec1 <- rec |> step_unorder(X2)

  rec1_trained <- prep(rec1, training = examples, verbose = FALSE)
  rec1_trans <- bake(rec1_trained, new_data = examples)

  expect_true(is.factor(rec1_trans$X2))
  expect_true(!is.ordered(rec1_trans$X2))

  expect_equal(levels(rec1_trans$X2), levels(examples$X2))
  expect_equal(as.character(rec1_trans$X2), as.character(examples$X2))
})

test_that("wrong vars", {
  rec2 <- rec |> step_unorder(X1, X2)
  expect_snapshot(prep(rec2, training = examples, verbose = FALSE))
  rec3 <- rec |> step_unorder(X1)
  expect_snapshot(prep(rec3, training = examples, verbose = FALSE))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec1 <- rec |>
    step_unorder(X2) |>
    update_role(X2, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec1_trained <- prep(rec1, training = examples, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec1_trained, new_data = examples[, 1, drop = FALSE])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_unorder(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_unorder(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_unorder(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~ X1 + X2, data = examples) |>
    step_unorder(X2)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  data$Species <- as.ordered(data$Species)

  rec <- recipe(~., data) |>
    step_unorder(Species) |>
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
