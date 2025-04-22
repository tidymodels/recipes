library(testthat)
library(recipes)

n <- 20

set.seed(752)
ex_dat <- data.frame(
  numbers = rnorm(n),
  fact = factor(sample(letters[1:3], n, replace = TRUE)),
  ord1 = factor(sample(LETTERS[1:3], n, replace = TRUE), ordered = TRUE),
  ord2 = factor(sample(LETTERS[4:8], n, replace = TRUE), ordered = TRUE),
  ord3 = factor(sample(LETTERS[10:20], n, replace = TRUE), ordered = TRUE)
)

ex_miss <- ex_dat
ex_miss$ord1[c(1, 5, 9)] <- NA
ex_miss$ord3[2] <- NA

score <- function(x) as.numeric(x)^2

test_that("linear scores", {
  rec1 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_ordinalscore(starts_with("ord"))
  rec1 <- prep(
    rec1,
    training = ex_dat,
    verbose = FALSE
  )
  rec1_scores <- bake(rec1, new_data = ex_dat)
  rec1_scores_NA <- bake(rec1, new_data = ex_miss)

  expect_identical(as.integer(ex_dat$ord1), rec1_scores$ord1)
  expect_identical(as.integer(ex_dat$ord2), rec1_scores$ord2)
  expect_identical(as.integer(ex_dat$ord3), rec1_scores$ord3)

  expect_identical(as.integer(ex_miss$ord1), rec1_scores_NA$ord1)
  expect_identical(as.integer(ex_miss$ord3), rec1_scores_NA$ord3)
})

test_that("nonlinear scores", {
  rec2 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_ordinalscore(starts_with("ord"), convert = score)
  rec2 <- prep(
    rec2,
    training = ex_dat,
    verbose = FALSE
  )
  rec2_scores <- bake(rec2, new_data = ex_dat)
  rec2_scores_NA <- bake(rec2, new_data = ex_miss)

  expect_equal(as.numeric(ex_dat$ord1)^2, rec2_scores$ord1)
  expect_equal(as.numeric(ex_dat$ord2)^2, rec2_scores$ord2)
  expect_equal(as.numeric(ex_dat$ord3)^2, rec2_scores$ord3)

  expect_equal(as.numeric(ex_miss$ord1)^2, rec2_scores_NA$ord1)
  expect_equal(as.numeric(ex_miss$ord3)^2, rec2_scores_NA$ord3)
})

test_that("bad spec", {
  rec3 <- recipe(~., data = ex_dat) |>
    step_ordinalscore(all_predictors())
  expect_snapshot(error = TRUE, prep(rec3, training = ex_dat, verbose = FALSE))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec1 <- recipe(~., data = ex_dat, strings_as_factors = FALSE) |>
    step_ordinalscore(starts_with("ord")) |>
    update_role(starts_with("ord"), new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  rec1 <- prep(
    rec1,
    training = ex_dat,
    verbose = FALSE
  )

  expect_snapshot(error = TRUE, bake(rec1, new_data = ex_dat[, 1:3]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ordinalscore(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_ordinalscore(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ordinalscore(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_ordinalscore(starts_with("ord"))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = ex_dat) |>
      step_ordinalscore(starts_with("ord"), convert = NULL) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  data$Species <- as.ordered(data$Species)
  rec <- recipe(~., data) |>
    step_ordinalscore(Species) |>
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
