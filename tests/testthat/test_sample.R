library(testthat)
library(recipes)
library(dplyr)
library(modeldata)
data(scat)
scat <- na.omit(scat)

# ------------------------------------------------------------------------------

context("dplyr sampling steps")

# ------------------------------------------------------------------------------

scat2 <- scat %>% mutate(row = 1:nrow(scat))
scat_rec <- recipe( ~ ., data = scat2)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  single_sample <-
    scat_rec %>%
    step_sample(size = 1) %>%
    prep(training = scat2) %>%
    juice() %>%
    nrow()
  expect_equal(single_sample, 1)

  full_sample <-
    scat_rec %>%
    step_sample(size = 0.99999) %>%
    prep(training = scat2) %>%
    juice() %>%
    nrow()
  expect_equal(full_sample, nrow(scat))

  half_sample <-
    scat_rec %>%
    step_sample(size = 0.5) %>%
    prep(training = scat2) %>%
    juice() %>%
    nrow()
  expect_equal(half_sample, ceiling(nrow(scat)/2))

  third_sample <-
    scat_rec %>%
    step_sample(size = 50) %>%
    prep(training = scat2) %>%
    juice() %>%
    nrow()
  expect_equal(third_sample, 50)

  whole_sample <-
    scat_rec %>%
    step_sample() %>%
    prep(training = scat2) %>%
    juice() %>%
    nrow()
  expect_equal(whole_sample, nrow(scat))

  smaller_scat <-
    scat_rec %>%
    step_sample() %>%
    prep(training = scat2 %>% slice(1:60))

  expect_equal(juice(smaller_scat) %>% nrow(), 60)
  expect_equal(bake(smaller_scat, scat2 %>% slice(61:nrow(scat))) %>% nrow(), 31)

  boot_sample <-
    scat_rec %>%
    step_sample(replace = TRUE) %>%
    prep(training = scat2) %>%
    juice() %>%
    pull(row) %>%
    table()
  expect_true(max(boot_sample) > 1)
  expect_equal(sum(boot_sample), nrow(scat))
})

test_that('bad input', {
  expect_error(scat_rec %>% step_sample(size = -1))
  expect_error(scat_rec %>% step_sample(size = "a"))
  expect_error(scat_rec %>% step_sample(replace = "a"))
})



test_that('printing', {
  rec <- scat_rec %>% step_sample()
  expect_output(print(rec))
  expect_output(prep(rec, training = scat2, verbose = TRUE))
})

