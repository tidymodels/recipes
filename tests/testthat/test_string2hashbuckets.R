library(testthat)
library(recipes)

summarize_buckets <- function(num_buckets) {
  rec <- recipe(~ Species, data = iris) %>%
    step_factor2string(Species) %>%
    step_string2hashbucket(Species, num_buckets = num_buckets) %>%
    prep(iris)
  bake(rec, iris) %>% count(Species)
}

test_that("string2hashbucket basic functionality works", {
  expect_equal(nrow(summarize_buckets(1)), 1)
  expect_equal(nrow(summarize_buckets(100)), 3)
})

test_that("string2hashbucket throws error for bad num_buckets", {
  expect_error(summarize_buckets(0),
               "'num_buckets' must be at least 1")
})

test_that("string2hashbucket error when try to transform non-character column", {
  expect_error(recipe(~ Species, data = iris) %>%
                 step_string2hashbucket(Species, num_buckets = 100) %>%
                 prep(iris),
               "The following variables are not character vectors: `Species`")
})
