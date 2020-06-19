library(testthat)
library(recipes)
library(modeldata)
data(scat)
scat <- na.omit(scat)

context("Formula stuff")

## -----------------------------------------------------------------------------

test_that('is trained?', {
  rec1 <- recipe(~ Age + Length, data = scat)
  expect_true(fully_trained(rec1))

  rec2 <- rec1 %>%
    step_sqrt(all_numeric()) %>%
    step_center(all_numeric())
  expect_false(fully_trained(rec2))

  rec3 <- prep(rec2, training = scat)
  expect_true(fully_trained(rec3))

  rec4 <- rec3 %>% step_scale(all_numeric())
  expect_false(fully_trained(rec4))

  rec5 <- prep(rec4, training = scat)
  expect_true(fully_trained(rec5))

})

test_that('formulas', {
  rec6 <- recipe(Species ~ ., data = scat %>% dplyr::select(Species, Age, Mass, Taper, Length))
  expect_equal(
    formula(rec6),
    as.formula(Species ~ Age + Mass + Taper + Length)
  )

  rec7 <- rec6 %>% step_rm(Age) %>% prep(scat)
  expect_equal(
    formula(rec7),
    as.formula(Species ~       Mass + Taper + Length)
  )

  rec8 <- recipe(~ ., data = scat %>% dplyr::select(Species, Age, Mass, Taper, Length))
  expect_equal(
    formula(rec8),
    as.formula( ~ Species + Age + Mass + Taper + Length)
  )

  rec9 <- recipe(Species + Age ~ ., data = scat %>% dplyr::select(Species, Age, Mass, Taper, Length))
  expect_equal(
    formula(rec9),
    as.formula(Species + Age ~ Mass + Taper + Length)
  )
})

test_that('bad args', {

  rec10 <- recipe(Species ~ Age + Mass, data = scat) %>%
    step_center(all_numeric())
  expect_error(formula(rec10))
})

