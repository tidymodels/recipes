library(recipes)
library(testthat)

n <- 200

set.seed(8575)
tr_dat <- data.frame(
  v = sample(letters[1:3], size = n, replace = TRUE),
  w = sample(LETTERS[1:2], size = n, replace = TRUE),
  x = factor(rep_len(month.abb, n)),
  y = factor(rep_len(month.name[-1], n), ordered = TRUE),
  z = factor(rep_len(month.name[-1], n), ordered = TRUE, levels = month.name),
  stringsAsFactors = FALSE
)

tr_bad <- tr_dat
levels(tr_bad$x) <- c(levels(tr_bad$x), "new")

te_dat <- data.frame(
  v = letters[1:5],
  w = LETTERS[1:5],
  x = factor(month.abb[1:5]),
  y = factor(month.name[1:5], ordered = TRUE),
  z = factor(month.name[1:5], ordered = TRUE, levels = month.name),
  stringsAsFactors = FALSE
)

te_miss <- te_dat
te_miss$y[1] <- NA
te_miss$z[1] <- NA

rec <- recipe(~., data = tr_dat)

test_that("basic functionality", {
  ex_1 <- recipe(~., data = tr_dat, strings_as_factors = FALSE) |>
    step_novel(all_predictors()) |>
    prep(tr_dat)

  ex_1_tr <- bake(ex_1, new_data = tr_dat)
  ex_1_te <- bake(ex_1, new_data = te_dat)

  all(ex_1_te$v[!(ex_1_te$v %in% letters[1:3])] == "new")

  expect_true(all(vapply(ex_1_tr, is.factor, logical(1))))
  expect_true(all(vapply(ex_1_te, is.factor, logical(1))))

  for (i in names(ex_1_tr)) {
    expect_true(
      all.equal(
        as.character(tr_dat[[i]]),
        as.character(ex_1_tr[[i]])
      )
    )
  }
  expect_true(
    all(ex_1_te$v[!(ex_1_te$v %in% letters[1:3])] == "new")
  )
  expect_true(
    all(ex_1_te$w[!(ex_1_te$w %in% LETTERS[1:2])] == "new")
  )
  expect_true(
    all(as.character(te_dat$x) == as.character(ex_1_te$x))
  )
  expect_true(ex_1_te$y[1] == "new")
  expect_true(
    all(as.character(te_dat$z[-1]) == as.character(ex_1_te$z[-1]))
  )
  expect_true(
    all(as.character(te_dat$z) == as.character(ex_1_te$z))
  )

  expect_true(is.ordered(ex_1_te$y))
  expect_true(is.ordered(ex_1_te$z))
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = iris) |>
      step_novel(all_predictors()) |>
      prep(iris)
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = tr_bad) |>
      step_novel(all_predictors()) |>
      prep(tr_bad)
  )
  expect_snapshot(
    rec |>
      step_novel(all_predictors(), new_level = letters) |>
      prep(),
    error = TRUE
  )
})

test_that("missing values", {
  ex_2 <- rec |>
    step_novel(all_predictors()) |>
    prep(training = tr_dat)
  ex_2_te <- bake(ex_2, new_data = te_miss)
  expect_equal(which(is.na(te_miss$y)), which(is.na(ex_2_te$y)))
  expect_equal(which(is.na(te_miss$z)), which(is.na(ex_2_te$z)))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  ex_1 <- recipe(~., data = tr_dat, strings_as_factors = FALSE) |>
    step_novel(x) |>
    update_role(x, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep(tr_dat)

  expect_snapshot(error = TRUE, bake(ex_1, new_data = tr_dat[, c(-3)]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_novel(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_novel(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_novel(rec)

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = tr_dat) |>
    step_novel(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_novel(all_nominal_predictors()) |>
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
