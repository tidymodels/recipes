library(testthat)
library(recipes)
library(dplyr)


context("Upsampling")

iris2 <- iris[-(1:45),]
iris2$Species[seq(6, 96, by = 5)] <- NA
iris2$Species2 <- sample(iris2$Species)
iris2$Species3 <- as.character(sample(iris2$Species))

rec <- recipe( ~ ., data = iris2)

test_that('basic usage', {
  rec1 <- rec %>%
    step_upsample(matches("Species$"), id = "")

  untrained <- tibble(
    terms = "matches(\"Species$\")", id = ""
  )

  expect_equivalent(untrained, tidy(rec1, number = 1))

  rec1_p <- prep(rec1, training = iris2)

  trained <- tibble(
    terms = "Species",
    id = ""
  )

  expect_equal(trained, tidy(rec1_p, number = 1))

  tr_xtab <- table(juice(rec1_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec1_p, new_data = iris2)$Species, useNA = "always")
  og_xtab <- table(iris2$Species, useNA = "always")

  expect_equal(max(tr_xtab), max(og_xtab))
  expect_equal(sum(is.na(juice(rec1_p)$Species)), max(og_xtab))
  expect_equal(te_xtab, og_xtab)

  expect_warning(prep(rec1, training = iris2), NA)
})

test_that('ratio value', {
  rec2 <- rec %>%
    step_upsample(matches("Species$"), ratio = .25)

  rec2_p <- prep(rec2, training = iris2)

  tr_xtab <- table(juice(rec2_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec2_p, new_data = iris2)$Species, useNA = "always")
  og_xtab <- table(iris2$Species, useNA = "always")

  expect_equal(min(tr_xtab), 10)
  expect_equal(sum(is.na(juice(rec2_p)$Species)),
               sum(is.na(iris2$Species)))
  expect_equal(te_xtab, og_xtab)
})


test_that('no skipping', {
  rec3 <- rec %>%
    step_upsample(matches("Species$"), skip = FALSE)

  rec3_p <- prep(rec3, training = iris2)

  tr_xtab <- table(juice(rec3_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec3_p, new_data = iris2)$Species, useNA = "always")
  og_xtab <- table(iris2$Species, useNA = "always")

  expect_equal(max(tr_xtab), max(og_xtab))
  expect_equal(te_xtab, tr_xtab)
})



test_that('bad data', {
  expect_error(
    rec %>%
      step_upsample(Sepal.Width) %>%
      prep(retain = TRUE)
  )
  expect_error(
    rec %>%
      step_upsample(Species3) %>%
      prep(strings_as_factors = FALSE)
  )
  expect_error(
    rec %>%
      step_upsample(Species, Species2) %>%
      prep(strings_as_factors = FALSE)
  )
})

test_that('printing', {
  rec4 <- rec %>%
    step_upsample(Species)

  expect_output(print(rec))
  expect_output(prep(rec4, training = iris2, verbose = TRUE))
})

test_that('`seed` produces identical sampling', {

  upsample_with_seed <- function(rec, seed = sample.int(10^5, 1)) {
    rec %>%
      step_upsample(Species, seed = seed) %>%
      prep(training = iris2) %>%
      juice() %>%
      pull(Petal.Width)
  }

  petal_width_1 <- upsample_with_seed(rec, seed = 1234)
  petal_width_2 <- upsample_with_seed(rec, seed = 1234)
  petal_width_3 <- upsample_with_seed(rec, seed = 12345)

  expect_equal(petal_width_1, petal_width_2)
  expect_false(identical(petal_width_1, petal_width_3))
})


test_that('ratio deprecation', {

  expect_message(
    new_rec <-
      rec %>%
      step_upsample(tidyselect::matches("Species$"), ratio = 2),
    "argument is now deprecated"
  )
  expect_equal(new_rec$steps[[1]]$over_ratio, 2)
})



test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_upsample(all_predictors())
  rec_param <- tunable.step_upsample(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})


