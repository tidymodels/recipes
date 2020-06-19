library(testthat)
library(recipes)
library(dplyr)
library(modeldata)
data(scat)
scat <- na.omit(scat)

# ------------------------------------------------------------------------------

context("dplyr mutate steps")

# ------------------------------------------------------------------------------

scat_rec <- recipe( ~ ., data = scat)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    scat_rec %>%
    step_mutate(
      dbl_width = Mass * 2,
      half_length = Age / 2
    )

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(
      dbl_width = Mass * 2,
      half_length = Age / 2
    )

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    mutate(
      dbl_width = Mass * 2,
      half_length = Age / 2
    )
  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
  const <- 9.077
  rec_1 <-
    scat_rec %>%
    step_mutate(new_var = Mass * const)

  prepped_1 <- prep(rec_1, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(new_var = Mass * const)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    scat_rec %>%
    step_mutate(new_var = Mass * !!const)

  prepped_2 <- prep(rec_2, training = scat %>% slice(1:75))

  rm(const)
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
    step_mutate() %>%
    prep(training = scat) %>%
    juice()
  expect_equal(no_inputs, scat)
})

test_that('printing', {
  rec <- scat_rec %>% step_mutate(x = 5)
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})

# ------------------------------------------------------------------------------

context("dplyr mutate_at steps")

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    scat_rec %>%
    step_mutate_at(Age, Taper, fn = log)

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(
      Age = log(Age),
      Taper = log(Taper)
    )

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    mutate(
      Age = log(Age),
      Taper = log(Taper)
    )
  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('mulitple functions', {
  rec <-
    scat_rec %>%
    step_mutate_at(Age, Taper, fn = list(a = log, b = sqrt))

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    mutate(
      Age_a = log(Age),
      Taper_a = log(Taper),
      Age_b = sqrt(Age),
      Taper_b = sqrt(Taper)
    )

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    mutate(
      Age_a = log(Age),
      Taper_a = log(Taper),
      Age_b = sqrt(Age),
      Taper_b = sqrt(Taper)
    )
  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})


test_that('no input', {
  expect_error(
    scat_rec %>%
      step_mutate_at() %>%
      prep(training = scat) %>%
      juice()
  )
})

test_that('printing', {
  rec <- scat_rec %>% step_mutate_at(Age, fn = log)
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})
