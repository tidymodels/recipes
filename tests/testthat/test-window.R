library(testthat)
library(recipes)

set.seed(5522)
sim_dat <- data.frame(x1 = (20:100) / 10)
n <- nrow(sim_dat)
sim_dat$y1 <- sin(sim_dat$x1) + rnorm(n, sd = 0.1)
sim_dat$y2 <- cos(sim_dat$x1) + rnorm(n, sd = 0.1)
sim_dat$x2 <- runif(n)
sim_dat$x3 <- rnorm(n)
sim_dat$fac <- sample(letters[1:3], size = n, replace = TRUE)

rec <- recipe(~., data = sim_dat)

test_that("error checks", {
  skip_if_not_installed("RcppRoll")

  expect_snapshot(error = TRUE, rec |> step_window(y1, size = 6) |> prep())
  expect_snapshot(error = TRUE, rec |> step_window(y1, size = NA) |> prep())
  expect_snapshot(
    error = TRUE,
    rec |> step_window(y1, size = NULL) |> prep()
  )
  expect_snapshot(error = TRUE, rec |> step_window(y1, statistic = "average"))
  expect_snapshot(error = TRUE, rec |> step_window(y1, size = 1) |> prep())
  expect_snapshot(error = TRUE, rec |> step_window(y1, size = 2) |> prep())
  expect_snapshot(error = TRUE, rec |> step_window(y1, size = -1) |> prep())
  expect_snapshot(
    rec |> step_window(y1, size = 3 + .Machine$double.eps) |> prep()
  )
  expect_snapshot(
    rec |> step_window(y1, size = 3 + 2 * .Machine$double.eps) |> prep(),
    error = TRUE
  )
  expect_snapshot(
    error = TRUE,
    prep(rec |> step_window(fac), training = sim_dat)
  )
  expect_snapshot(
    error = TRUE,
    prep(rec |> step_window(y1, size = 1000L), training = sim_dat) |> prep()
  )
  bad_names <- rec |>
    step_window(starts_with("y"), names = "only_one_name")
  expect_snapshot(error = TRUE, prep(bad_names, training = sim_dat))
})

test_that("basic moving average", {
  skip_if_not_installed("RcppRoll")
  simple_ma <- rec |>
    step_window(starts_with("y"))
  simple_ma <- prep(simple_ma, training = sim_dat)
  simple_ma_res <- bake(simple_ma, new_data = sim_dat)
  expect_equal(names(sim_dat), names(simple_ma_res))

  for (i in 2:(n - 1)) {
    expect_equal(simple_ma_res$y1[i], mean(sim_dat$y1[(i - 1):(i + 1)]))
    expect_equal(simple_ma_res$y2[i], mean(sim_dat$y2[(i - 1):(i + 1)]))
  }
  expect_equal(simple_ma_res$y1[1], mean(sim_dat$y1[1:3]))
  expect_equal(simple_ma_res$y2[1], mean(sim_dat$y2[1:3]))
  expect_equal(simple_ma_res$y1[n], mean(sim_dat$y1[(n - 2):n]))
  expect_equal(simple_ma_res$y2[n], mean(sim_dat$y2[(n - 2):n]))
})

test_that("creating new variables", {
  skip_if_not_installed("RcppRoll")
  new_names <- rec |>
    step_window(
      starts_with("y"),
      names = paste0("new", 1:2),
      role = "predictor"
    )
  new_names <- prep(new_names, training = sim_dat)
  new_names_res <- bake(new_names, new_data = sim_dat)

  simple_ma <- rec |>
    step_window(starts_with("y"))
  simple_ma <- prep(simple_ma, training = sim_dat)
  simple_ma_res <- bake(simple_ma, new_data = sim_dat)

  expect_equal(new_names_res$new1, simple_ma_res$y1)
  expect_equal(new_names_res$new2, simple_ma_res$y2)
})

test_that("na_rm argument works for step_window", {
  skip_if_not_installed("RcppRoll")

  sim_dat_na <- sim_dat
  sim_dat_na[7, 2:3] <- NA

  simple_ma_no_rm_na <- recipe(~., data = sim_dat_na) |>
    step_window(starts_with("y"), na_rm = FALSE) |>
    prep() |>
    bake(new_data = NULL)

  simple_ma_rm_na <- recipe(~., data = sim_dat_na) |>
    step_window(starts_with("y"), na_rm = TRUE) |>
    prep() |>
    bake(new_data = NULL)

  expect_false(anyNA(simple_ma_rm_na$y1))
  expect_false(anyNA(simple_ma_rm_na$y2))

  exp_rm_na <- simple_ma_rm_na
  exp_rm_na[6:8, 2:3] <- NA

  expect_equal(
    simple_ma_no_rm_na,
    exp_rm_na
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_window(all_predictors(), outcome = "Species")
  rec_param <- tunable.step_window(rec$steps[[1]])
  expect_equal(rec_param$name, c("statistic", "size"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("check_name() is used", {
  skip_if_not_installed("RcppRoll")

  dat <- mtcars
  dat$new_value <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_window(mpg, names = "new_value")

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("error on too large window size", {
  skip_if_not_installed("RcppRoll")

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_window(mpg, size = 999) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("RcppRoll")
  rec <- rec |>
    step_window(x1) |>
    update_role(x1, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = sim_dat)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = sim_dat[, -1]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_window(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_window(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_window(rec, size = 3L)

  expect <- tibble(
    terms = character(),
    statistic = character(),
    size = integer(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("RcppRoll")
  new_names <- c("new_y1")

  rec <- recipe(~y1, data = sim_dat) |>
    step_window(y1, names = "new_y1", keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~y1, data = sim_dat) |>
    step_window(y1, names = "new_y1", keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("y1", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("RcppRoll")
  rec <- recipe(~y1, data = sim_dat) |>
    step_window(y1, names = "new_y1")

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = sim_dat)
  )
})

test_that("printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_window(rec)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_window(
      all_predictors(),
      statistic = hardhat::tune(),
      size = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("0 and 1 rows data work in bake method", {
  # step_window() requires rows to apply itself to

  expect_true(TRUE)
})
