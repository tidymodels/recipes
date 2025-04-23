library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")
biomass <- as_tibble(biomass)

means <- vapply(biomass[, 3:7], mean, c(mean = 0))

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass
)

test_that("working correctly", {
  standardized <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "center")

  cent_tibble_un <-
    tibble(
      terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
      value = rep(na_dbl, 5),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized, 1), cent_tibble_un)

  standardized_trained <- prep(standardized, training = biomass)

  cent_tibble_tr <-
    tibble(
      terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
      value = unname(means),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized_trained, 1), cent_tibble_tr)

  expect_equal(standardized_trained$steps[[1]]$means, means)
})

test_that("single predictor", {
  standardized <- rec |>
    step_center(carbon)

  standardized_trained <- prep(standardized, training = biomass)
  results <- bake(standardized_trained, biomass)

  exp_res <- biomass[, 3:8]
  exp_res$carbon <- exp_res$carbon - mean(exp_res$carbon)

  expect_equal(results, exp_res[, colnames(results)])
})

test_that("na_rm argument works for step_center", {
  mtcars_na <- mtcars
  mtcars_na[1, 1:4] <- NA

  rec_no_na_rm <- recipe(~., data = mtcars_na) |>
    step_center(all_predictors(), na_rm = FALSE) |>
    prep()

  rec_na_rm <- recipe(~., data = mtcars_na) |>
    step_center(all_predictors(), na_rm = TRUE) |>
    prep()

  exp_no_na_rm <- vapply(mtcars_na, FUN = mean, FUN.VALUE = numeric(1))
  exp_na_rm <- vapply(
    mtcars_na,
    FUN = mean,
    FUN.VALUE = numeric(1),
    na.rm = TRUE
  )

  expect_equal(
    tidy(rec_no_na_rm, 1)$value,
    unname(exp_no_na_rm)
  )

  expect_equal(
    tidy(rec_na_rm, 1)$value,
    unname(exp_na_rm)
  )
})

test_that("centering with case weights", {
  mtcars_freq <- mtcars
  mtcars_freq$cyl <- frequency_weights(mtcars_freq$cyl)

  rec <-
    recipe(mpg ~ ., mtcars_freq) |>
    step_center(all_numeric_predictors()) |>
    prep()

  expect_equal(
    tidy(rec, number = 1)[["value"]],
    unname(averages(mtcars_freq[, -c(1, 2)], mtcars_freq$cyl))
  )

  expect_snapshot(rec)

  mtcars_imp <- mtcars
  mtcars_imp$wt <- importance_weights(mtcars_imp$wt)

  rec <-
    recipe(mpg ~ ., mtcars_imp) |>
    step_center(all_numeric_predictors()) |>
    prep()

  expect_equal(
    tidy(rec, number = 1)[["value"]],
    unname(averages(mtcars_imp[, -c(1, 6)], NULL))
  )

  expect_snapshot(rec)
})

test_that("warns when NaN is returned due to Inf or -Inf", {
  rec <- recipe(~., data = data.frame(x = c(2, 3, 4, Inf))) |>
    step_center(x)
  expect_snapshot(prep(rec))

  rec <- recipe(~., data = data.frame(x = c(2, 3, 4, -Inf))) |>
    step_center(x)
  expect_snapshot(prep(rec))
})
# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  std <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    update_role(
      carbon,
      hydrogen,
      oxygen,
      nitrogen,
      sulfur,
      new_role = "potato"
    ) |>
    update_role_requirements(role = "potato", bake = FALSE)

  std_trained <- prep(std, training = biomass)

  expect_snapshot(error = TRUE, bake(std_trained, new_data = biomass[, 1:2]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_center(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_center(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_center(rec)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- rec |>
    step_center(carbon)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_center(mpg, disp) |>
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
