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
  x1 = 0.2874304685,
  x2 = NA,
  x3 = NA,
  x4 = 0.06115365314
)
exp_dat <- structure(
  list(
    x1 = c(
      -0.48855792533959,
      0.295526451871788,
      -0.66306066037752,
      2.18444062220084,
      0.45714544418559,
      -0.650762952308473,
      0.639934327981261,
      0.94795174900382,
      0.745877376631664,
      -0.199443408020842,
      2.05013184840922,
      0.526004196848377,
      -0.484073411411316,
      -1.5846209165316,
      1.46827089088108,
      0.0555044880684726,
      0.0848273579417863,
      1.21733702306844,
      1.05470177834901,
      0.76793945044649
    ),
    x2 = c(
      1.0881660755694,
      1.27854953038913,
      13.4111208085756,
      -0.502676325196487,
      1.61335666257264,
      -17.8161848705567,
      -6.41867035287092,
      -0.679924106156326,
      -2.09139367300257,
      2.39267901359744,
      0.736008721758276,
      -9.72878791903891,
      2.57950278065913,
      -18.5856192870844,
      -0.726185004156987,
      -2.40967012205861,
      -2.5362046143702,
      -16.8595975858421,
      0.909069940992826,
      1.31031417340121
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
      -0.0299153493217198,
      -0.00545480495048682,
      -0.605467890118739,
      0.771791879612809,
      -0.763649380406524,
      0.872804671752781,
      1.38894407918253,
      -0.537364454265797,
      -0.482864603899052,
      -0.0227886234018179,
      -1.25797709152009,
      -0.995703197045091,
      0.102163556869708,
      -0.246753343931442,
      -1.7395729395129,
      0.104247324965852,
      -1.15077903230011,
      0.48306309307708,
      1.99265865015763,
      -0.747338829803379
    )
  ),
  .Names = c("x1", "x2", "x3", "x4"),
  row.names = c(NA, -20L),
  class = "data.frame"
)

test_that("simple Box Cox", {
  rec <- recipe(~., data = ex_dat) |>
    step_BoxCox(x1, x2, x3, x4)

  bc_tibble_un <-
    tibble(
      terms = c("x1", "x2", "x3", "x4"),
      value = rep(na_dbl, 4),
      id = rec$steps[[1]]$id
    )
  expect_equal(bc_tibble_un, tidy(rec, number = 1))

  # Capture warnings
  suppressWarnings(
    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  )
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

  # Capture warnings
  expect_snapshot(
    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  )
})

test_that("warnings", {
  expect_snapshot(
    recipe(~., data = exp_dat) |>
      step_BoxCox(x1) |>
      prep()
  )

  expect_snapshot(
    recipe(~ mpg + disp, data = mtcars) |>
      step_BoxCox(mpg, disp) |>
      prep() |>
      bake(new_data = tibble(mpg = -1, disp = -1))
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = ex_dat) |>
    step_BoxCox(x1, x2, x3, x4) |>
    update_role(x1, x2, x3, x4, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  # Capture warnings
  suppressWarnings(
    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  )

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = ex_dat[, 1:2]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_BoxCox(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_BoxCox(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_BoxCox(rec)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = ex_dat) |>
    step_BoxCox(x1, x2, x3, x4)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_BoxCox(mpg, disp) |>
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
