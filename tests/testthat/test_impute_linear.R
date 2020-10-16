library(testthat)
library(modeldata)

context("linear regression imputation")
data(ames)

ames_dat <- ames %>%
  select(Lot_Frontage, Lot_Area, Total_Bsmt_SF) %>%
  mutate(Lot_Frontage = na_if(Lot_Frontage, 0)) %>%
  mutate(Total_Bsmt_SF = na_if(Total_Bsmt_SF, 0))

tg_dat <- ToothGrowth %>%
  mutate(dose = na_if(dose, 0.5))


test_that("Does the imputation (no NAs), and does it correctly.", {

  missing_ind <- which(is.na(ames_dat$Lot_Frontage), arr.ind = T)

  imputed <- recipe(head(ames_dat)) %>%
    step_impute_linear(Lot_Frontage, impute_with = c("Lot_Area")) %>%
    prep(ames_dat) %>%
    juice %>%
    pull(Lot_Frontage) %>%
    .[missing_ind]

  lm_predicted <- lm(Lot_Frontage ~ Lot_Area, data = ames_dat) %>%
    predict(newdata = ames_dat[missing_ind, ]) %>%
    unname

  expect_equal(imputed, lm_predicted)
  expect_equal(sum(is.na(imputed)), 0)

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
