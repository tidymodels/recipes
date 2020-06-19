library(testthat)
library(recipes)
library(dplyr)
library(modeldata)
data(scat)
scat <- na.omit(scat)

# ------------------------------------------------------------------------------

context("dplyr slice steps")

# ------------------------------------------------------------------------------

scat_rec <- recipe( ~ ., data = scat)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    scat_rec %>%
    step_slice(1:5)

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    slice(1:5)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:nrow(scat))
  dplyr_test <- dplyr_test[, names(rec_train)]
  rec_test <- bake(prepped, scat %>% slice(76:nrow(scat)))
  expect_equal(dplyr_test, rec_test)
})


test_that('skip = FALSE', {
  rec <-
    scat_rec %>%
    step_slice(1:5, skip = FALSE)

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    slice(1:5)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:nrow(scat)) %>%
    slice(1:5)
  rec_test <- bake(prepped, scat %>% slice(76:nrow(scat)))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
  values <- 1:5
  rec_1 <-
    scat_rec %>%
    step_slice(values)

  prepped_1 <- prep(rec_1, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    slice(values)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  expect_error(
    rec_2 <-
      scat_rec %>%
      step_slice(!!values),
    regexp = NA
  )

  prepped_2 <- prep(rec_2, training = scat %>% slice(1:75))

  rm(values)
  expect_error(prep(rec_1, training = scat %>% slice(1:75)))
  expect_error(
    prepped_2 <- prep(rec_2, training = scat %>% slice(1:75)),
    regexp = NA
  )
  rec_2_train <- juice(prepped_2)
  expect_equal(dplyr_train, rec_2_train)
})


test_that('no input', {
  no_inputs <-
    scat_rec %>%
    step_slice() %>%
    prep(training = scat) %>%
    juice()
  expect_equal(no_inputs, scat)
})


test_that('printing', {
  rec <- scat_rec %>% step_slice(1:2)
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})

