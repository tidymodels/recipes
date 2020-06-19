library(testthat)
library(recipes)
library(dplyr)
library(modeldata)
data(scat)
scat <- na.omit(scat)
scat$Species[c(1, 5, 10)] <- NA

context("Down-sampling")


rec <- recipe( ~ ., data = scat)

test_that('basic usage', {
  expect_warning(
    rec1 <- rec %>%
      step_downsample(Species, id = ""),
    "themis"
  )

  untrained <- tibble(
    terms = "Species",
    id = ""
  )

  expect_equivalent(untrained, tidy(rec1, number = 1))

  rec1_p <- prep(rec1, training = scat)

  trained <- tibble(
    terms = "Species",
    id = ""
  )

  expect_equal(trained, tidy(rec1_p, number = 1))


  tr_xtab <- table(juice(rec1_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec1_p, new_data = scat)$Species, useNA = "always")
  og_xtab <- table(scat$Species, useNA = "always")

  expect_equal(max(tr_xtab), 17)
  expect_equal(sum(is.na(juice(rec1_p)$Species)), 3)
  expect_equal(te_xtab, og_xtab)

  expect_warning(prep(rec1, training = scat), NA)
})

test_that('ratio value', {
  rec2 <- rec %>%
    step_downsample(Species, under_ratio = 2)

  rec2_p <- prep(rec2, training = scat)

  tr_xtab <- table(juice(rec2_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec2_p, new_data = scat)$Species, useNA = "always")
  og_xtab <- table(scat$Species, useNA = "always")

  expect_equal(max(tr_xtab), 34)
  expect_equal(sum(is.na(juice(rec2_p)$Species)), 3)
  expect_equal(te_xtab, og_xtab)
})


test_that('no skipping', {
  rec3 <- rec %>%
    step_downsample(Species, skip = FALSE)

  rec3_p <- prep(rec3, training = scat)

  tr_xtab <- table(juice(rec3_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec3_p, new_data = scat)$Species, useNA = "always")
  og_xtab <- table(scat$Species, useNA = "always")

  expect_equal(max(tr_xtab), 17)
  expect_equal(te_xtab, tr_xtab)
})



test_that('bad data', {
  expect_error(
    rec %>%
      step_downsample(Mass) %>%
      prep(retain = TRUE)
  )
  expect_error(
    rec %>%
      step_downsample(Species3) %>%
      prep(strings_as_factors = FALSE)
  )
  expect_error(
    rec %>%
      step_downsample(Species, Species2) %>%
      prep(strings_as_factors = FALSE)
  )
})

test_that('printing', {
  rec4 <- rec %>%
    step_downsample(Species)

  expect_output(print(rec))
  expect_output(prep(rec4, training = scat, verbose = TRUE))
})

test_that('`seed` produces identical sampling', {

  downsample_with_seed <- function(rec, seed = sample.int(10^5, 1)) {
    rec %>%
      step_downsample(Species, seed = seed) %>%
      prep(training = scat) %>%
      juice() %>%
      pull(Length)
  }

  petal_width_1 <- downsample_with_seed(rec, seed = 1234)
  petal_width_2 <- downsample_with_seed(rec, seed = 1234)
  petal_width_3 <- downsample_with_seed(rec, seed = 12345)

  expect_equal(petal_width_1, petal_width_2)
  expect_false(identical(petal_width_1, petal_width_3))
})


test_that('ratio deprecation', {

  expect_message(
    new_rec <-
      rec %>%
      step_downsample(tidyselect::matches("Species$"), ratio = 2),
    "argument is now deprecated"
  )
  expect_equal(new_rec$steps[[1]]$under_ratio, 2)
})



test_that('tunable', {
  rec <-
    recipe(~ ., data = scat) %>%
    step_downsample(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_downsample(rec$steps[[1]])
  expect_equal(rec_param$name, c("under_ratio"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})


