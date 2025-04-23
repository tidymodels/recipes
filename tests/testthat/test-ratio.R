library(testthat)
library(recipes)

n <- 20
ex_dat <- data.frame(
  x1 = -1:8,
  x2 = 1,
  x3 = c(1:9, NA),
  x4 = 11:20,
  x5 = letters[1:10]
)

rec <- recipe(~ x1 + x2 + x3 + x4 + x5, data = ex_dat)

test_that("1:many", {
  rec1 <- rec |>
    step_ratio(x1, denom = all_numeric(), id = "")

  exp_un_1 <- tibble(
    terms = "x1",
    denom = "all_numeric()",
    id = ""
  )
  expect_equal(tidy(rec1, number = 1), exp_un_1)

  rec1 <- prep(rec1, ex_dat, verbose = FALSE)
  obs1 <- bake(rec1, ex_dat)
  res1 <- tibble(
    x1_o_x2 = ex_dat$x1 / ex_dat$x2,
    x1_o_x3 = ex_dat$x1 / ex_dat$x3,
    x1_o_x4 = ex_dat$x1 / ex_dat$x4
  )
  for (i in names(res1)) {
    expect_equal(res1[i], obs1[i])
  }

  exp_tr_1 <- tibble(
    terms = rep("x1", 3),
    denom = c("x2", "x3", "x4"),
    id = ""
  )
  expect_equal(tidy(rec1, number = 1), exp_tr_1)
})

test_that("many:1", {
  rec2 <- rec |>
    step_ratio(all_numeric(), denom = x1, id = "")

  exp_un_2 <- tibble(
    terms = "all_numeric()",
    denom = "x1",
    id = ""
  )
  expect_equal(tidy(rec2, number = 1), exp_un_2)

  rec2 <- prep(rec2, ex_dat, verbose = FALSE)
  obs2 <- bake(rec2, ex_dat)
  res2 <- tibble(
    x2_o_x1 = ex_dat$x2 / ex_dat$x1,
    x3_o_x1 = ex_dat$x3 / ex_dat$x1,
    x4_o_x1 = ex_dat$x4 / ex_dat$x1
  )
  for (i in names(res2)) {
    expect_equal(res2[i], obs2[i])
  }

  exp_tr_2 <- tibble(
    terms = c("x2", "x3", "x4"),
    denom = rep("x1", 3),
    id = ""
  )
  expect_equal(tidy(rec2, number = 1), exp_tr_2)
})

test_that("many:many", {
  rec3 <- rec |>
    step_ratio(all_numeric(), denom = all_numeric(), id = "")

  exp_un_3 <- tibble(
    terms = "all_numeric()",
    denom = "all_numeric()",
    id = ""
  )
  expect_equal(tidy(rec3, number = 1), exp_un_3)

  rec3 <- prep(rec3, ex_dat, verbose = FALSE)
  obs3 <- bake(rec3, ex_dat)
  res3 <- tibble(
    x2_o_x1 = ex_dat$x2 / ex_dat$x1,
    x3_o_x1 = ex_dat$x3 / ex_dat$x1,
    x4_o_x1 = ex_dat$x4 / ex_dat$x1,
    x1_o_x2 = ex_dat$x1 / ex_dat$x2,
    x3_o_x2 = ex_dat$x3 / ex_dat$x2,
    x4_o_x2 = ex_dat$x4 / ex_dat$x2,
    x1_o_x3 = ex_dat$x1 / ex_dat$x3,
    x2_o_x3 = ex_dat$x2 / ex_dat$x3,
    x4_o_x3 = ex_dat$x4 / ex_dat$x3,
    x1_o_x4 = ex_dat$x1 / ex_dat$x4,
    x2_o_x4 = ex_dat$x2 / ex_dat$x4,
    x3_o_x4 = ex_dat$x3 / ex_dat$x4
  )
  for (i in names(res3)) {
    expect_equal(res3[i], obs3[i])
  }

  exp_tr_3 <- tidyr::crossing(
    terms = paste0("x", 1:4),
    denom = paste0("x", 1:4)
  )
  exp_tr_3 <- exp_tr_3[exp_tr_3$terms != exp_tr_3$denom, ]
  exp_tr_3$id <- ""

  expect_equal(tidy(rec3, number = 1), exp_tr_3)
})

test_that("wrong type", {
  rec4 <- rec |>
    step_ratio(x1, denom = all_predictors())
  expect_snapshot(error = TRUE, prep(rec4, ex_dat, verbose = FALSE))

  rec5 <- rec |>
    step_ratio(all_predictors(), denom = x1)
  expect_snapshot(error = TRUE, prep(rec5, ex_dat, verbose = FALSE))

  rec6 <- rec |>
    step_ratio(all_predictors(), denom = all_predictors())
  expect_snapshot(error = TRUE, prep(rec6, ex_dat, verbose = FALSE))
})

test_that("check_name() is used", {
  dat <- mtcars
  dat$mpg_o_disp <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_ratio(mpg, denom = disp)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("recipes_argument_select() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_ratio(disp, denom = NULL) |>
      prep()
  )
})

test_that("addition of recipes_argument_select() is backwards compatible", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_ratio(all_predictors(), denom = disp) |>
    prep()

  exp <- bake(rec, mtcars)

  rec$steps[[1]]$denom <- rlang::new_quosures(
    list(
      rlang::new_quosure(quote(disp))
    )
  )

  expect_identical(
    bake(rec, mtcars),
    exp
  )

  rec_old <- recipe(mpg ~ ., data = mtcars) |>
    step_ratio(all_predictors(), denom = denom_vars(disp)) |>
    prep()

  expect_identical(
    bake(rec_old, mtcars),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec1 <- rec |>
    step_ratio(x1, denom = all_numeric()) |>
    update_role(x1, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec1 <- prep(rec1, ex_dat, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(rec1, ex_dat[, 2:5]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ratio(rec, denom = mpg)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_ratio(rec1, denom = mpg)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ratio(rec, denom = mpg)

  expect <- tibble(terms = character(), denom = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("mpg_o_disp")

  rec <- recipe(~ mpg + disp, mtcars) |>
    step_ratio(mpg, denom = disp, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~ mpg + disp, mtcars) |>
    step_ratio(mpg, denom = disp, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("mpg", "disp", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~ mpg + disp, mtcars) |>
    step_ratio(mpg, denom = disp)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = mtcars)
  )
})

test_that("printing", {
  rec <- recipe(~ x1 + x2 + x3 + x4 + x5, data = ex_dat) |>
    step_ratio(all_numeric(), denom = all_numeric())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(~ mpg + disp, mtcars) |>
      step_ratio(mpg, denom = disp, naming = NULL) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_ratio(disp, mpg, denom = carb) |>
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
