library(testthat)
library(recipes)
skip_if_not_installed("modeldata")
data(credit_data, package = "modeldata")

set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[in_training, ]
credit_te <- credit_data[-in_training, ]

test_that("simple modes", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mode(Status, Home, Marital, id = "")
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = credit_te)

  expect_equal(te_imputed$Status, credit_te$Status)
  home_exp <- rep(
    recipes:::mode_est(credit_tr$Home),
    sum(is.na(credit_te$Home))
  )
  home_exp <- factor(home_exp, levels = levels(credit_te$Home))
  expect_equal(
    te_imputed$Home[is.na(credit_te$Home)],
    home_exp
  )
  marital_exp <- rep(
    recipes:::mode_est(credit_tr$Marital),
    sum(is.na(credit_te$Marital))
  )
  marital_exp <- factor(marital_exp, levels = levels(credit_te$Marital))
  expect_equal(
    te_imputed$Marital[is.na(credit_te$Marital)],
    marital_exp
  )

  modes <- vapply(
    credit_tr[, c("Status", "Home", "Marital")],
    recipes:::mode_est,
    character(1)
  )
  imp_tibble_un <-
    tibble(
      terms = c("Status", "Home", "Marital"),
      value = rep(NA_character_, 3),
      id = ""
    )
  imp_tibble_tr <-
    tibble(
      terms = c("Status", "Home", "Marital"),
      value = modes,
      id = ""
    )

  expect_equal(as.data.frame(tidy(impute_rec, 1)), as.data.frame(imp_tibble_un))
  expect_equal(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_tr))
})

test_that("non-nominal", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mode(Assets, Job)
  expect_snapshot(
    error = TRUE,
    prep(impute_rec, training = credit_tr, verbose = FALSE)
  )
})

test_that("all NA values", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mode(Status, Home)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)
  imputed_te <- bake(imputed, credit_te |> mutate(Status = factor(NA)))
  expect_equal(
    imputed$steps[[1]]$modes[["Status"]],
    as.character(unique(imputed_te$Status))
  )
})

test_that("can bake recipes with no ptype", {
  imputed <- recipe(Price ~ ., data = credit_tr) |>
    step_impute_mode(Status, Home) |>
    prep(credit_tr, verbose = FALSE)

  imputed$steps[[1]]$ptype <- NULL

  expect_snapshot(
    imputed_te <- bake(imputed, credit_te)
  )
})

test_that('case weights', {
  fake_data <- tibble(
    x1 = rep(letters[c(1:4, NA)], c(50, 40, 30, 20, 10)),
    x2 = frequency_weights(1:150)
  )

  impute_rec <- recipe(~., data = fake_data) |>
    step_impute_mode(x1, id = "")
  imputed <- prep(impute_rec, training = fake_data, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = fake_data)

  imp_tibble_tr <- fake_data |>
    mutate(x2 = as.double(x2)) |>
    count(value = x1, wt = x2) |>
    slice_max(n, n = 1) |>
    mutate(terms = "x1", id = "") |>
    select(terms, value, id)

  expect_equal(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_tr))

  expect_snapshot(imputed)

  # ----------------------------------------------------------------------------

  fake_data <- tibble(
    x1 = rep(letters[c(1:4, NA)], c(50, 40, 30, 20, 10)),
    x2 = importance_weights(1:150)
  )

  impute_rec <- recipe(~., data = fake_data) |>
    step_impute_mode(x1, id = "")
  imputed <- prep(impute_rec, training = fake_data, verbose = FALSE)
  te_imputed <- bake(imputed, new_data = fake_data)

  imp_tibble_tr <- fake_data |>
    count(value = x1) |>
    slice_max(n, n = 1) |>
    mutate(terms = "x1", id = "") |>
    select(terms, value, id)

  expect_equal(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_tr))

  expect_snapshot(imputed)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(Price ~ ., data = credit_tr)

  impute_rec <- rec |>
    step_impute_mode(Marital) |>
    update_role(Marital, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  imputed <- prep(impute_rec, training = credit_tr, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(imputed, new_data = credit_te[, c(-6)]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_mode(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_impute_mode(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_mode(rec)

  expect <- tibble(terms = character(), value = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(Price ~ ., data = credit_tr) |>
    step_impute_mode(Status, Home, Marital)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_impute_bag(Species) |>
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
