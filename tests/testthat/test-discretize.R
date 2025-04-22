library(testthat)
library(recipes)

r_version <- function() paste0("R", getRversion()[, 1:2])

ex_tr <- data.frame(
  x1 = 1:100,
  x2 = rep(1:5, each = 20),
  x3 = factor(rep(letters[1:2], each = 50))
)
ex_tr_mis <- ex_tr
ex_tr_mis$x1[2] <- NA
ex_tr_mis$x3[10] <- NA

ex_te <- data.frame(x1 = c(1, 50, 101, NA))

lvls_breaks_4 <- c(
  "[missing]",
  "[-Inf,25.8]",
  "(25.8,50.5]",
  "(50.5,75.2]",
  "(75.2, Inf]"
)
lvls_breaks_4_bin <- c("bin_missing", "bin1", "bin2", "bin3", "bin4")

test_that("default args", {
  bin_1 <- discretize(ex_tr$x1, prefix = NULL)
  pred_1 <- predict(bin_1, ex_te$x1)
  exp_1 <- factor(lvls_breaks_4[c(2, 3, 5, 1)], levels = lvls_breaks_4)
  expect_equal(pred_1, exp_1)

  bin_1 <- discretize(ex_tr$x1)
  pred_1 <- predict(bin_1, ex_te$x1)
  exp_1 <- factor(
    c("bin1", "bin2", "bin4", "bin_missing"),
    levels = lvls_breaks_4_bin
  )
  expect_equal(pred_1, exp_1)
})

test_that("NA values", {
  bin_2 <- discretize(ex_tr$x1, keep_na = FALSE, prefix = NULL)
  pred_2 <- predict(bin_2, ex_te$x1)
  exp_2 <- factor(lvls_breaks_4[c(2, 3, 5, NA)], levels = lvls_breaks_4[-1])
  expect_equal(pred_2, exp_2)

  bin_2 <- discretize(ex_tr$x1, keep_na = FALSE)
  pred_2 <- predict(bin_2, ex_te$x1)
  exp_2 <- factor(c("bin1", "bin2", "bin4", NA), levels = lvls_breaks_4_bin[-1])
  expect_equal(pred_2, exp_2)
})

test_that("bad values", {
  expect_snapshot(error = TRUE, discretize(letters))
})

test_that("printing of discretize()", {
  expect_snapshot(discretize(1:100))
  expect_snapshot(discretize(1:100, cuts = 6))
  expect_snapshot(discretize(1:100, keep_na = FALSE))

  expect_snapshot(
    res <- discretize(1:2)
  )
  expect_snapshot(res)
})

test_that("NA values from out of range", {
  bin_3 <- discretize(ex_tr$x1, keep_na = FALSE, infs = FALSE, prefix = NULL)
  pred_3 <- predict(bin_3, ex_te$x1)
  exp_3 <- factor(
    c("[1,25.8]", "(25.8,50.5]", NA, NA),
    levels = c("[1,25.8]", "(25.8,50.5]", "(50.5,75.2]", "(75.2,100]")
  )
  expect_equal(pred_3, exp_3)

  bin_3 <- discretize(ex_tr$x1, keep_na = FALSE, infs = FALSE)
  pred_3 <- predict(bin_3, ex_te$x1)
  exp_3 <- factor(c("bin1", "bin2", NA, NA), levels = lvls_breaks_4_bin[-1])
  expect_equal(pred_3, exp_3)
})

test_that("NA values with step_discretize (issue #127)", {
  iris_na <- iris
  iris_na$sepal_na <- iris_na$Sepal.Length
  iris_na$sepal_na[1:5] <- NA

  disc_values <-
    discretize(
      iris_na$sepal_na,
      min.unique = 2,
      cuts = 2,
      keep_na = TRUE,
      na.rm = TRUE
    )

  # We expect na.rm to be overwritten
  opts <- list(min.unique = 2, cuts = 2, keep_na = TRUE, na.rm = FALSE)

  rec <- recipe(~., data = iris_na) |>
    step_discretize(sepal_na, options = opts) |>
    prep(training = iris_na)

  expect_equal(rec$steps[[1]]$objects$sepal_na, disc_values)
})

test_that("tidys", {
  rec <- recipe(~., data = ex_tr) |>
    step_discretize(x1, id = "")

  tidy_exp_un <- tibble(
    terms = "x1",
    value = NA_real_,
    id = ""
  )
  expect_equal(tidy(rec, 1), tidy_exp_un)

  rec_trained <- prep(rec, training = ex_tr)
  br <- rec_trained$steps[[1]]$objects$x1$breaks
  tidy_exp_tr <- tibble(
    terms = rep("x1", length(br)),
    value = br,
    id = ""
  )
  expect_equal(tidy(rec_trained, 1), tidy_exp_tr)
})

test_that("multiple column prefix", {
  set.seed(1234)
  example_data <- tibble(
    x1 = rnorm(1000),
    x2 = rnorm(1000)
  )
  expect_snapshot(
    recipe(~., data = example_data) |>
      step_discretize(x1, x2, options = list(prefix = "hello")) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = example_data) |>
      step_discretize(x1, x2, options = list(labels = "hello")) |>
      prep(),
    variant = r_version()
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = ex_tr) |>
      step_discretize(x1, num_breaks = 1) |>
      prep()
  )
  expect_snapshot(
    recipe(~., data = ex_tr) |>
      step_discretize(x1, num_breaks = 100) |>
      prep()
  )
  expect_snapshot(
    recipe(~., data = ex_tr) |>
      step_discretize(x1, options = list(prefix = "@$")) |>
      prep()
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_discretize(all_predictors())
  rec_param <- tunable.step_discretize(rec$steps[[1]])
  expect_equal(rec_param$name, c("min_unique", "num_breaks"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("war when less breaks are generated", {
  expect_snapshot(
    tmp <- discretize(c(rep(1, 50), 1:50), cuts = 5, min_unique = 1)
  )
})

test_that("bake works predicting on only NAs (#1350)", {
  rec <- recipe(~mpg, data = mtcars) |>
    step_discretize(mpg, min_unique = 4) |>
    prep()

  exp_levels <- rec |>
    bake(data.frame(mpg = numeric(0))) |>
    pull() |>
    levels()

  expect_identical(
    rec |>
      bake(data.frame(mpg = NA)) |>
      pull(mpg),
    factor(NA, exp_levels)
  )

  expect_identical(
    rec |>
      bake(data.frame(mpg = c(NA, NA))) |>
      pull(mpg),
    factor(c(NA, NA), exp_levels)
  )
})

test_that("check_options() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(~mpg, data = mtcars) |>
      step_discretize(mpg, options = TRUE) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(cyl ~ ., mtcars)
  rec <- step_discretize(rec, mpg, min_unique = 3) |>
    update_role(mpg, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  rec <- prep(rec, mtcars)

  expect_snapshot(error = TRUE, bake(rec, new_data = mtcars[, 2:ncol(mtcars)]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_discretize(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_discretize(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_discretize(rec)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_tr) |>
    step_discretize(x1)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_discretize(
      all_predictors(),
      min_unique = hardhat::tune(),
      num_breaks = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_discretize(disp, num_breaks = 0) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_discretize(disp, min_unique = -1) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_discretize(mpg, min_unique = 3) |>
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
