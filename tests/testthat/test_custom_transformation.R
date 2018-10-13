library(testthat)
library(rlang)
library(recipes)
library(dplyr)
library(purrr)
library(tibble)
library(tidyselect)

# fejl, når output ikke kan konverteres til tibble

context("Testing custom transformation")

# generate data.
set.seed(1)
df <- tibble(a = rnorm(100),
             b = rnorm(100),
             c = rnorm(100))

# define prep helper function, that computes means and standard deviations
# for all variables in a data set.
compute_means_sd <- function(x, na.rm = FALSE, trim = 0) {

  map(x, ~ list(mean = mean(.x, na.rm = na.rm, trim = trim),
                sd = sd(.x)))

}

# define bake helper function, that subtracts k means from the variable, and
# then divides by the standard deviation.
center_scale <- function(x, prep_output, k) {

  newdata <- select(x, names(prep_output))

  map2(.x = newdata,
       .y = prep_output,
       ~ (.x - k * .y$mean) / .y$sd)
}

# create initial recipe.
rec <- recipe(df)

test_that('end-to-end results for center-scale transformation', {

  # benchmark recipe.
  rec_center_scale <- rec %>%
    step_center(everything()) %>%
    step_scale(everything()) %>%
    prep(retain = TRUE)

  # centering and scaling with 'custom transformation' recipe.
  rec_custom <- rec %>%
    step_custom_transformation(everything(),
                               prep_function = compute_means_sd,
                               prep_options = list(na.rm = TRUE, trim = 0),
                               bake_function = center_scale,
                               bake_options = list(k = 1),
                               bake_how = "replace") %>%
    prep(retain = TRUE)

  expect_identical(juice(rec_center_scale), juice(rec_custom))

})

test_that('output from prep', {

  rec_custom <- rec %>%
    step_custom_transformation(everything(),
                               prep_function = compute_means_sd,
                               prep_options = list(na.rm = TRUE, trim = 0.2),
                               bake_function = center_scale,
                               bake_options = list(k = 1),
                               bake_how = "replace") %>%
    prep(retain = TRUE)

  expect_identical(
  rec_custom$steps[[1]]$prep_output,
  purrr::map(df, ~ list(mean = mean(.x, trim = 0.2), sd = sd(.x)))
  )

})

test_that('output from bake', {

  # bake_how = 'bind_cols'
  rec_custom <- rec %>%
    step_custom_transformation(everything(),
                               bake_function = function (x) {transmute(x, d = 1)},
                               bake_how = "bind_cols") %>%
    prep(retain = TRUE)

  expect_identical(juice(rec_custom), mutate(df, d = 1))
  expect_identical(names(juice(rec_custom)), letters[1:4])

  # bake_how = 'replace'
  rec_custom <- rec %>%
    step_custom_transformation(everything(),
                               bake_function = function (x) {transmute(x, d = 1)},
                               bake_how = "replace") %>%
    prep(retain = TRUE)

  expect_identical(juice(rec_custom), transmute(df, d = 1))

  rec_custom <- rec %>%
    step_custom_transformation(a, b,
                               bake_function = function (x) {transmute(x, d = 1)},
                               bake_how = "replace") %>%
    prep(retain = TRUE)

  expect_identical(juice(rec_custom), df %>% select(-a, -b) %>% mutate(d = 1))

})

test_that('expected errors for incorrect baking output', {

  # bake_how = 'bind_cols'.
  rec_custom <- rec %>%
    step_custom_transformation(a, b,
                               bake_function = function (x) {
                                 top_n(x, 10)
                                 },
                               bake_how = "bind_cols")

  expect_error(prep(rec_custom, retain = TRUE))

  # bake_how = 'replace'.
  rec_custom <- rec %>%
    step_custom_transformation(a, b,
                               bake_function = function (x) {
                                 top_n(x, 10)
                               },
                               bake_how = "replace")

  expect_error(prep(rec_custom, retain = TRUE))

})

test_that('printing', {
  rec_custom <- rec %>%
    step_custom_transformation(everything(),
                               prep_function = compute_means_sd,
                               bake_function = center_scale,
                               bake_options = list(k = 5),
                               bake_how = "bind_cols")
  expect_output(print(rec_custom))
  expect_output(print(prep(rec_custom)))
})

