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

test_that("works correctly", {
  standardized <- rec |>
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "scale")

  scal_tibble_un <-
    tibble(
      terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
      value = rep(na_dbl, 5),
      id = standardized$steps[[1]]$id
    )
  scal_tibble_un$id <- standardized$steps[[1]]$id

  expect_equal(tidy(standardized, 1), scal_tibble_un)

  standardized_trained <- prep(standardized, training = biomass)

  scal_tibble_tr <-
    tibble(
      terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
      value = unname(sds),
      id = standardized$steps[[1]]$id
    )

  expect_equal(
    tidy(standardized_trained, 1),
    scal_tibble_tr
  )

  expect_equal(standardized_trained$steps[[1]]$sds, sds)
})

test_that("scale by factor of 1 or 2", {
  standardized <- rec |>
    step_scale(
      carbon,
      hydrogen,
      oxygen,
      nitrogen,
      sulfur,
      id = "scale",
      factor = 2
    )

  standardized_trained <- prep(standardized, training = biomass)

  scal_tibble_tr <-
    tibble(
      terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
      value = unname(sds * 2),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized_trained, 1), scal_tibble_tr)

  expect_equal(standardized_trained$steps[[1]]$sds, 2 * sds)

  expect_snapshot(
    not_recommended_standardized_input <- rec |>
      step_scale(carbon, id = "scale", factor = 3) |>
      prep(training = biomass)
  )
})

test_that("single predictor", {
  standardized <- rec |>
    step_scale(hydrogen)

  standardized_trained <- prep(standardized, training = biomass)
  results <- bake(standardized_trained, biomass)

  exp_res <- biomass[, 3:8]
  exp_res$hydrogen <- exp_res$hydrogen / sd(exp_res$hydrogen)

  expect_equal(results, exp_res[, colnames(results)])
})

test_that("na_rm argument works for step_scale", {
  mtcars_na <- mtcars
  mtcars_na[1, 1:4] <- NA

  expect_snapshot({
    rec_no_na_rm <- recipe(~., data = mtcars_na) |>
      step_scale(all_predictors(), na_rm = FALSE) |>
      prep()
  })

  rec_na_rm <- recipe(~., data = mtcars_na) |>
    step_scale(all_predictors(), na_rm = TRUE) |>
    prep()

  exp_no_na_rm <- vapply(mtcars_na, FUN = sd, FUN.VALUE = numeric(1))
  exp_na_rm <- vapply(mtcars_na, FUN = sd, FUN.VALUE = numeric(1), na.rm = TRUE)

  expect_equal(
    tidy(rec_no_na_rm, 1)$value,
    unname(exp_no_na_rm)
  )

  expect_equal(
    tidy(rec_na_rm, 1)$value,
    unname(exp_na_rm)
  )
  expect_snapshot(
    rec_no_na_rm <- recipe(~., data = mtcars_na) |>
      step_scale(all_predictors(), na_rm = "FALSE") |>
      prep(),
    error = TRUE
  )
})

test_that("warns on zv", {
  rec1 <- step_scale(rec_zv, all_numeric_predictors())
  expect_snapshot(prep(rec1))
})

test_that("warns when NaN is returned", {
  rec1 <- rec |>
    step_log(sulfur) |>
    step_scale(sulfur)
  expect_snapshot(prep(rec1))
})

test_that("scaling with case weights", {
  mtcars_freq <- mtcars
  mtcars_freq$cyl <- frequency_weights(mtcars_freq$cyl)

  rec <-
    recipe(mpg ~ ., mtcars_freq) |>
    step_scale(all_numeric_predictors()) |>
    prep()

  expect_equal(
    tidy(rec, number = 1)[["value"]],
    unname(sqrt(variances(mtcars_freq[, -c(1, 2)], mtcars_freq$cyl)))
  )

  expect_snapshot(rec)

  mtcars_imp <- mtcars
  mtcars_imp$wt <- importance_weights(mtcars_imp$wt)

  rec <-
    recipe(mpg ~ ., mtcars_imp) |>
    step_scale(all_numeric_predictors()) |>
    prep()

  expect_equal(
    tidy(rec, number = 1)[["value"]],
    unname(sqrt(variances(mtcars_imp[, -c(1, 6)], NULL)))
  )

  expect_snapshot(rec)
})

test_that("doesn't destroy sparsity", {
  mtcars$vs <- sparsevctrs::as_sparse_double(mtcars$vs)
  mtcars$am <- sparsevctrs::as_sparse_integer(mtcars$am)
  rec <- recipe(~ am + vs, data = mtcars) |>
    step_scale(am, vs)

  rec_trained <- prep(rec, training = mtcars, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = mtcars)

  expect_true(all(vapply(rec_trans, sparsevctrs::is_sparse_double, logical(1))))

  expect_true(.recipes_preserve_sparsity(rec$steps[[1]]))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  std <- rec |>
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
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
  rec <- step_scale(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_scale(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_scale(rec)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_scale(disp, wt)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_scale(all_predictors()) |>
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
