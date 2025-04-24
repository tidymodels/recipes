library(testthat)
skip_if_not_installed("modeldata")

data(ames, package = "modeldata")

ames_dat <- ames |>
  select(Lot_Frontage, Lot_Area, Total_Bsmt_SF) |>
  mutate(Lot_Frontage = na_if(Lot_Frontage, 0)) |>
  mutate(Total_Bsmt_SF = na_if(Total_Bsmt_SF, 0))

tg_dat <- ToothGrowth |>
  mutate(dose = na_if(dose, 0.5))

test_that("Does the imputation (no NAs), and does it correctly.", {
  missing_ind <- which(is.na(ames_dat$Lot_Frontage), arr.ind = TRUE)

  imputed <- recipe(head(ames_dat)) |>
    step_impute_linear(Lot_Frontage, impute_with = Lot_Area) |>
    prep(ames_dat) |>
    bake(new_data = NULL) |>
    pull(Lot_Frontage)

  imputed <- imputed[missing_ind]

  lm_predicted <- lm(Lot_Frontage ~ Lot_Area, data = ames_dat) |>
    predict(newdata = ames_dat[missing_ind, ]) |>
    unname()

  expect_equal(imputed, lm_predicted)
  expect_equal(sum(is.na(imputed)), 0)
})

test_that("All NA values", {
  imputed <- recipe(head(ames_dat)) |>
    step_impute_linear(Lot_Frontage, impute_with = Lot_Area) |>
    prep(ames_dat)

  imputed_te <- bake(imputed, ames_dat |> mutate(Lot_Frontage = NA))

  lm_predicted <- lm(Lot_Frontage ~ Lot_Area, data = ames_dat) |>
    predict(newdata = ames_dat) |>
    unname()

  expect_equal(unname(imputed_te$Lot_Frontage), lm_predicted)
  expect_equal(sum(is.na(imputed_te$Lot_Frontage)), 0)
})

test_that("Returns correct models.", {
  imputed <- recipe(head(ames_dat)) |>
    step_impute_linear(
      Lot_Frontage,
      Total_Bsmt_SF,
      impute_with = Lot_Area
    ) |>
    prep(ames_dat)

  expect_equal(length(imputed$steps[[1]]$models), 2)
  expect_equal(
    sort(names(imputed$steps[[1]]$models)),
    c("Lot_Frontage", "Total_Bsmt_SF")
  )
})

test_that("Fails when one of the variables to impute is non-numeric.", {
  expect_snapshot(
    error = TRUE,
    recipe(tg_dat) |>
      step_impute_linear(supp, impute_with = len) |>
      prep(tg_dat)
  )
  expect_snapshot(
    error = TRUE,
    recipe(tg_dat) |>
      step_impute_linear(supp, dose, impute_with = len) |>
      prep(tg_dat)
  )
})

test_that("Maintain data type", {
  ames_integer <- ames
  ames_integer$TotRms_AbvGrd[1:10] <- NA_integer_
  integer_rec <- recipe(~., data = ames_integer) |>
    step_impute_linear(
      TotRms_AbvGrd,
      impute_with = c(Bedroom_AbvGr, Gr_Liv_Area)
    ) |>
    prep()
  expect_true(
    is.integer(
      bake(integer_rec, ames_integer, TotRms_AbvGrd) |> pull(TotRms_AbvGrd)
    )
  )
})

test_that("case weights", {
  missing_ind <- which(is.na(ames_dat$Lot_Frontage), arr.ind = TRUE)

  ames_dat_cw <- ames_dat |>
    mutate(Total_Bsmt_SF = frequency_weights(Total_Bsmt_SF))

  rec_prepped <- recipe(head(ames_dat_cw)) |>
    step_impute_linear(Lot_Frontage, impute_with = Lot_Area) |>
    prep(ames_dat_cw)

  imputed <- rec_prepped |>
    bake(new_data = NULL) |>
    pull(Lot_Frontage)

  imputed <- imputed[missing_ind]

  lm_predicted <- lm(
    Lot_Frontage ~ Lot_Area,
    data = ames_dat,
    weights = Total_Bsmt_SF
  ) |>
    predict(newdata = ames_dat[missing_ind, ]) |>
    unname()

  expect_equal(imputed, lm_predicted)
  expect_equal(sum(is.na(imputed)), 0)

  expect_snapshot(rec_prepped)

  # ----------------------------------------------------------------------------

  missing_ind <- which(is.na(ames_dat$Lot_Frontage), arr.ind = TRUE)

  ames_dat_cw <- ames_dat |>
    mutate(Total_Bsmt_SF = importance_weights(Total_Bsmt_SF))

  rec_prepped <- recipe(head(ames_dat_cw)) |>
    step_impute_linear(Lot_Frontage, impute_with = Lot_Area) |>
    prep(ames_dat_cw)

  imputed <- rec_prepped |>
    bake(new_data = NULL) |>
    pull(Lot_Frontage)

  imputed <- imputed[missing_ind]

  lm_predicted <- lm(Lot_Frontage ~ Lot_Area, data = ames_dat) |>
    predict(newdata = ames_dat[missing_ind, ]) |>
    unname()

  expect_equal(imputed, lm_predicted)
  expect_equal(sum(is.na(imputed)), 0)

  expect_snapshot(rec_prepped)
})

test_that("impute_with errors with nothing selected", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_impute_linear(all_predictors(), impute_with = NULL) |>
      prep()
  )
})

test_that("warns if impute_with columns contains missing values", {
  mtcars$mpg[3] <- NA
  mtcars$disp[3] <- NA
  expect_snapshot(
    tmp <- recipe(~., data = mtcars) |>
      step_impute_linear(mpg, impute_with = disp) |>
      prep()
  )
})

test_that("errors if there are no rows without missing values", {
  mtcars$am <- NA
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_impute_linear(all_predictors()) |>
      prep()
  )
})

test_that("recipes_argument_select() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_impute_linear(disp, impute_with = NULL) |>
      prep()
  )
})

test_that("addition of recipes_argument_select() is backwards compatible", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_impute_linear(disp) |>
    prep()

  exp <- bake(rec, mtcars)

  rec$steps[[1]]$impute_with <- rlang::new_quosures(
    list(
      rlang::new_quosure(
        quote(all_predictors())
      )
    )
  )

  expect_identical(
    bake(rec, mtcars),
    exp
  )

  rec_old <- recipe(mpg ~ ., data = mtcars) |>
    step_impute_linear(disp, impute_with = imp_vars(all_predictors())) |>
    prep()

  expect_identical(
    bake(rec_old, mtcars),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(head(ames_dat)) |>
    step_impute_linear(Lot_Frontage, impute_with = Lot_Area) |>
    update_role(Lot_Frontage, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep(ames_dat)

  expect_snapshot(
    error = TRUE,
    bake(rec, new_data = ames_dat[, 2:3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_linear(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_impute_linear(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_linear(rec)

  expect <- tibble(terms = character(), model = list(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(ames_dat) |>
    step_impute_linear(Lot_Frontage, impute_with = Lot_Area)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_impute_linear(disp, mpg) |>
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
