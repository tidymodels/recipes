library(testthat)
library(recipes)
library(dplyr)
library(modeldata)
data(scat)
scat <- na.omit(scat)

# ------------------------------------------------------------------------------

context("dplyr rename steps")

# ------------------------------------------------------------------------------

scat_rec <- recipe( ~ ., data = scat)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    scat_rec %>%
    step_rename(
      popcorn = Mass,
      plum = Age
    )

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    rename(
      popcorn = Mass,
      plum = Age
    )

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    rename(
      popcorn = Mass,
      plum = Age
    )
  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('no input', {
  no_inputs <-
    scat_rec %>%
    step_rename() %>%
    prep(training = scat) %>%
    juice()
  expect_equal(no_inputs, scat)
})

test_that('printing', {
  rec <- scat_rec %>% step_rename(wat = Species)
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})

# ------------------------------------------------------------------------------

context("dplyr rename_at steps")

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    scat_rec %>%
    step_rename_at(contains("Length"), fn = ~ tolower(.))

  prepped <- prep(rec, training = scat %>% slice(1:75))

  dplyr_train <-
    scat %>%
    as_tibble() %>%
    slice(1:75) %>%
    rename_at(vars(contains("Length")), ~ tolower(.))

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    scat %>%
    as_tibble() %>%
    slice(76:150) %>%
    rename_at(vars(contains("Length")), ~ tolower(.))
  rec_test <- bake(prepped, scat %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('mulitple functions', {
  rec <-
    scat_rec %>%
    step_rename_at(contains("Length"), fn = list(a = log, b = sqrt))

  expect_error(prep(rec, training = scat %>% slice(1:75)))

})


test_that('no input', {
  expect_error(
    scat_rec %>%
      step_rename_at() %>%
      prep(training = scat) %>%
      juice(composition = "data.frame")
  )
})

test_that('printing', {
  rec <- scat_rec %>% step_rename_at(Age, fn = tolower)
  expect_output(print(rec))
  expect_output(prep(rec, training = scat, verbose = TRUE))
})

