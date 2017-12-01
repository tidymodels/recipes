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

  set.seed(1)
  df <- data.frame(V1 = sample(letters, 1000, replace = TRUE),
                   stringsAsFactors = FALSE)
  expect_identical(recipe(~ V1, data = df) %>%
    step_string2hashbucket(V1, num_buckets = 5) %>%
    prep(df, stringsAsFactors = FALSE) %>%
    bake(df) %>%
    dplyr::count(V1) %>%
    dplyr::pull(V1),
    0:4L)
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
