library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")
biomass <- as_tibble(biomass)

means <- vapply(biomass[, 3:7], mean, c(mean = 0))
sds <- vapply(biomass[, 3:7], sd, c(sd = 0))

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass
)

biomass['zero_variance'] <- 1
rec_zv <- recipe(
  HHV ~ +carbon + hydrogen + oxygen + nitrogen + sulfur + zero_variance,
  data = biomass
)

test_that("correct means and std devs for step_normalize", {
  standardized <- rec |>
    step_normalize(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "norm")

  vrs <- c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur")
  norm_tibble_un <-
    tibble(
      terms = vrs,
      statistic = rep(na_chr, 5),
      value = rep(na_dbl, 5),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized, 1), norm_tibble_un)

  standardized_trained <- prep(standardized, training = biomass)

  norm_tibble_tr <-
    tibble(
      terms = c(vrs, vrs),
      statistic = rep(c("mean", "sd"), each = 5),
      value = unname(c(means, sds)),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized_trained, 1), norm_tibble_tr)
})

test_that("step_normalize works with 1 column (#963)", {
  standardized <- rec |>
    step_normalize(carbon, id = "norm")

  standardized_trained <- prep(standardized, training = biomass)

  norm_tibble_tr <-
    tibble(
      terms = c("carbon", "carbon"),
      statistic = c("mean", "sd"),
      value = unname(c(means[["carbon"]], sds[["carbon"]])),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized_trained, 1), norm_tibble_tr)
})

test_that("na_rm argument works for step_normalize", {
  mtcars_na <- mtcars
  mtcars_na[1, 1:4] <- NA

  expect_snapshot(
    rec_no_na_rm <- recipe(~., data = mtcars_na) |>
      step_normalize(all_predictors(), na_rm = FALSE) |>
      prep()
  )

  expect_snapshot(
    recipe(~., data = mtcars_na) |>
      step_normalize(all_predictors(), na_rm = 2) |>
      prep(),
    error = TRUE
  )

  rec_na_rm <- recipe(~., data = mtcars_na) |>
    step_normalize(all_predictors(), na_rm = TRUE) |>
    prep()

  exp_no_na_rm <- c(
    vapply(mtcars_na, FUN = mean, FUN.VALUE = numeric(1)),
    vapply(mtcars_na, FUN = sd, FUN.VALUE = numeric(1))
  )
  exp_na_rm <- c(
    vapply(mtcars_na, FUN = mean, FUN.VALUE = numeric(1), na.rm = TRUE),
    vapply(mtcars_na, FUN = sd, FUN.VALUE = numeric(1), na.rm = TRUE)
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

test_that("warns on zv", {
  rec1 <- step_normalize(rec_zv, all_numeric_predictors())
  expect_snapshot(prep(rec1))
})

test_that("normalizing with case weights", {
  mtcars_freq <- mtcars
  mtcars_freq$cyl <- frequency_weights(mtcars_freq$cyl)

  rec <-
    recipe(mpg ~ ., mtcars_freq) |>
    step_normalize(all_numeric_predictors()) |>
    prep()

  expect_equal(
    rec$steps[[1]]$means,
    averages(mtcars_freq[, -c(1, 2)], mtcars_freq$cyl)
  )

  expect_equal(
    rec$steps[[1]]$sds,
    sqrt(variances(mtcars_freq[, -c(1, 2)], mtcars_freq$cyl))
  )

  expect_snapshot(rec)

  mtcars_imp <- mtcars
  mtcars_imp$wt <- importance_weights(mtcars_imp$wt)

  rec <-
    recipe(mpg ~ ., mtcars_imp) |>
    step_normalize(all_numeric_predictors()) |>
    prep()

  expect_equal(
    rec$steps[[1]]$means,
    averages(mtcars_imp[, -c(1, 6)], NULL)
  )

  expect_equal(
    rec$steps[[1]]$sds,
    sqrt(variances(mtcars_imp[, -c(1, 6)], NULL))
  )

  expect_snapshot(rec)
})

test_that("warns when NaN is returned due to Inf or -Inf", {
  rec <- recipe(~., data = data.frame(x = c(2, 3, 4, Inf))) |>
    step_normalize(x)
  expect_snapshot(prep(rec))

  rec <- recipe(~., data = data.frame(x = c(2, 3, 4, -Inf))) |>
    step_normalize(x)
  expect_snapshot(prep(rec))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  std <- rec |>
    step_normalize(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
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
  rec <- step_normalize(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_normalize(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_normalize(rec)

  expect <- tibble(
    terms = character(),
    statistic = character(),
    value = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_normalize(disp, wt)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_normalize(all_numeric_predictors()) |>
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
