library(testthat)
library(recipes)
library(dplyr)
library(modeldata)
data(scat)
scat <- na.omit(scat)

# ------------------------------------------------------------------------------

context("dplyr filter steps")

# ------------------------------------------------------------------------------

scat_rec <- recipe( ~ ., data = scat)

# ------------------------------------------------------------------------------

test_that('basic usage - skip = FALSE', {
  rec <-
    scat_rec %>%
    step_filter(Age > 3, Species == "bobcat", skip = FALSE)

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::filter(Age > 3, Species == "bobcat")

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::filter(Age > 3, Species == "bobcat")
  dplyr_test <- dplyr_test[, names(rec_train)]

  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})


test_that('skip = FALSE', {
  rec <-
    scat_rec %>%
    step_filter(Age > 3, Species == "bobcat", skip = FALSE)

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::filter(Age > 3, Species == "bobcat")

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::filter(Age > 3, Species == "bobcat")
  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
  values <- c("coyote", "gray_fox")
  rec_1 <-
    scat_rec %>%
    step_filter(Age > 3, Species  %in% values)

  prepped_1 <- prep(rec_1, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    filter(Age > 3, Species  %in% values)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    scat_rec %>%
    step_filter(Age > 3, Species  %in% !!values)

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
    step_filter() %>%
    prep(training = scat) %>%
    juice()
  scat2 <- scat
  scat2$Species <- factor(scat2$Species)
  expect_equal(no_inputs, scat2)
})


test_that('printing', {
  rec <- scat_rec %>% step_filter(Age > 3)
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})

