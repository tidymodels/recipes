library(testthat)
library(recipes)

n <- 20
set.seed(1)
ex_dat <- data.frame(
  x1 = exp(rnorm(n, mean = .1)),
  x2 = 1 / rnorm(n),
  x3 = rep(1:2, each = n / 2),
  x4 = rexp(n)
)

## from `car` package
exp_lambda <- c(
  x1 = -0.2727204451,
  x2 = 1.139292543,
  x3 = NA,
  x4 = -1.012702061
)
exp_dat <- structure(
  list(
    x1 = c(
      0.435993557749438,
      0.754696454247318,
      0.371327932207827,
      1.46113017436327,
      0.82204097731098,
      0.375761562702297,
      0.89751975937422,
      1.02175936118846,
      0.940739811377902,
      0.54984302797741,
      1.41856737837093,
      0.850587387615876,
      0.437701618670981,
      0.112174615510591,
      1.21942112715274,
      0.654589551748501,
      0.666780580127795,
      1.12625135443351,
      1.0636850911955,
      0.949680956411546
    ),
    x2 = c(
      1.15307873387121,
      1.36532999080347,
      17.4648439780388,
      -0.487746797875704,
      1.74452440065935,
      -13.3640721541574,
      -5.35805967319061,
      -0.653901985285932,
      -1.90735599477338,
      2.65253432454371,
      0.76771137336975,
      -7.79484535687973,
      2.87484976680907,
      -13.8738947581599,
      -0.696856395842167,
      -2.17745353101028,
      -2.28384276604207,
      -12.7261652971783,
      0.95585544349634,
      1.40099012093008
    ),
    x3 = c(
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      2L,
      2L,
      2L,
      2L,
      2L,
      2L,
      2L,
      2L,
      2L,
      2L
    ),
    x4 = c(
      0.49061104973894,
      0.49670370366879,
      0.338742419511653,
      0.663722100577351,
      0.296260662322359,
      0.681346128666408,
      0.757581280603711,
      0.357148961119583,
      0.371872889850153,
      0.49239057672598,
      0.173259524331095,
      0.235933290139909,
      0.52297977893566,
      0.434927187456966,
      0.0822501770191215,
      0.523479652016858,
      0.197977570919824,
      0.608108816144845,
      0.821913792446345,
      0.300608495427594
    )
  ),
  .Names = c("x1", "x2", "x3", "x4"),
  row.names = c(
    NA,
    -20L
  ),
  class = "data.frame"
)

test_that("simple YJ trans", {
  rec <- recipe(~., data = ex_dat) |>
    step_YeoJohnson(x1, x2, x3, x4, id = "")

  yj_tibble_un <-
    tibble(
      terms = c("x1", "x2", "x3", "x4"),
      value = rep(na_dbl, 4),
      id = ""
    )
  expect_equal(yj_tibble_un, tidy(rec, number = 1))

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat)

  expect_equal(
    names(exp_lambda)[!is.na(exp_lambda)],
    names(rec_trained$steps[[1]]$lambdas)
  )
  expect_equal(
    exp_lambda[!is.na(exp_lambda)],
    rec_trained$steps[[1]]$lambdas,
    tolerance = .001
  )
  expect_equal(as.matrix(exp_dat), as.matrix(rec_trans), tolerance = .05)
})

test_that("missing data", {
  ex_dat$x1[1] <- NA
  rec_true <- recipe(~., data = ex_dat) |>
    step_YeoJohnson(x1, x2, x3, x4)

  expect_no_error(prep(rec_true, training = ex_dat, verbose = FALSE))

  rec_false <- recipe(~., data = ex_dat) |>
    step_YeoJohnson(x1, x2, x3, x4, na_rm = FALSE)

  expect_snapshot(
    error = TRUE,
    prep(rec_false, training = ex_dat, verbose = FALSE)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = ex_dat) |>
    step_YeoJohnson(x1, x2, x3, x4, id = "") |>
    update_role(x1, x2, x3, x4, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = ex_dat[, 1:2]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_YeoJohnson(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_YeoJohnson(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_YeoJohnson(rec)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_YeoJohnson(x1, x2, x3, x4)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = ex_dat) |>
      step_YeoJohnson(x1, x2, x3, x4, na_rm = "yes") |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = ex_dat) |>
      step_YeoJohnson(x1, x2, x3, x4, num_unique = "yes") |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = ex_dat) |>
      step_YeoJohnson(x1, x2, x3, x4, limits = NA_real_) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_YeoJohnson(all_predictors()) |>
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
