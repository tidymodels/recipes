library(testthat)
library(modeldata)

data(ames)

ames_dat <- ames %>%
  select(Lot_Frontage, Lot_Area, Total_Bsmt_SF) %>%
  mutate(Lot_Frontage = na_if(Lot_Frontage, 0)) %>%
  mutate(Total_Bsmt_SF = na_if(Total_Bsmt_SF, 0))

tg_dat <- ToothGrowth %>%
  mutate(dose = na_if(dose, 0.5))


test_that("Does the imputation (no NAs), and does it correctly.", {

  missing_ind <- which(is.na(ames_dat$Lot_Frontage), arr.ind = TRUE)

  imputed <- recipe(head(ames_dat)) %>%
    step_impute_linear(Lot_Frontage, impute_with = c("Lot_Area")) %>%
    prep(ames_dat) %>%
    juice %>%
    pull(Lot_Frontage) %>%
    .[missing_ind]

  lm_predicted <- lm(Lot_Frontage ~ Lot_Area, data = ames_dat) %>%
    predict(newdata = ames_dat[missing_ind, ]) %>%
    unname()

  expect_equal(imputed, lm_predicted)
  expect_equal(sum(is.na(imputed)), 0)

})

test_that("All NA values", {

  imputed <- recipe(head(ames_dat)) %>%
    step_impute_linear(Lot_Frontage, impute_with = c("Lot_Area")) %>%
    prep(ames_dat)

  imputed_te <- bake(imputed, ames_dat %>% mutate(Lot_Frontage = NA))

  lm_predicted <- lm(Lot_Frontage ~ Lot_Area, data = ames_dat) %>%
    predict(newdata = ames_dat) %>%
    unname()

  expect_equal(unname(imputed_te$Lot_Frontage), lm_predicted)
  expect_equal(sum(is.na(imputed_te$Lot_Frontage)), 0)

})


test_that("Returns correct models.", {

  imputed <- recipe(head(ames_dat)) %>%
    step_impute_linear(Lot_Frontage, Total_Bsmt_SF, impute_with = c("Lot_Area")) %>%
    prep(ames_dat)

  expect_equal(length(imputed$steps[[1]]$models), 2)
  expect_equal(
    sort(names(imputed$steps[[1]]$models)),
    c("Lot_Frontage", "Total_Bsmt_SF")
  )

})

test_that("Fails when one of the variables to impute is non-numeric.", {
  expect_error(
    recipe(tg_dat) %>%
      step_impute_linear(supp, impute_with = c("len")) %>%
      prep(tg_dat)
  )
  expect_error(
    recipe(tg_dat) %>%
      step_impute_linear(supp, dose, impute_with = c("len")) %>%
      prep(tg_dat)
  )
})


test_that("Maintain data type", {
 ames_integer <- ames
 ames_integer$TotRms_AbvGrd[1:10] <- NA_integer_
 integer_rec <- recipe(~ ., data = ames_integer) %>%
   step_impute_linear(TotRms_AbvGrd, impute_with = vars(Bedroom_AbvGr, Gr_Liv_Area)) %>%
   prep()
 expect_true(
   is.integer(bake(integer_rec, ames_integer, TotRms_AbvGrd) %>% pull(TotRms_AbvGrd))
 )
})


test_that('Prints.', {
  imputed <- recipe(ames_dat) %>%
    step_impute_linear(Lot_Frontage, impute_with = imp_vars(Lot_Area))

  expect_output(print(imputed))
  expect_output(prep(imputed, training = ames_dat, verbose = TRUE))
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

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_linear(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
