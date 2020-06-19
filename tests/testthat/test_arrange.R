library(testthat)
library(recipes)
library(dplyr)
library(modeldata)

data(scat)
scat <- na.omit(scat)

# ------------------------------------------------------------------------------

context("dplyr arrange steps")

# ------------------------------------------------------------------------------

scat_rec <- recipe( ~ ., data = scat)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    scat_rec %>%
    step_arrange(desc(Age), 1/Taper)

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::arrange(desc(Age), 1/Taper)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::arrange(desc(Age), 1/Taper)
  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
  sort_vars <- c("Age", "Taper")
  sort_vars <- syms(sort_vars)
  rec_1 <-
    scat_rec %>%
    step_arrange(!!!sort_vars)

  prepped_1 <- prep(rec_1, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    arrange(Age, Taper)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

})

test_that('no input', {
  no_inputs <-
    scat_rec %>%
    step_arrange() %>%
    prep(training = scat) %>%
    juice()
  expect_equal(no_inputs, scat)
})

test_that('printing', {
  rec <- scat_rec %>% step_arrange(Age)
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})

