library(testthat)
library(recipes)

data(okc)
okc <- okc[1:20,]
okc$diet <- factor(okc$diet)
okc_rec <- recipe(~ ., data = okc)

is_unq <- function(x) length(unique(x)) == 1

test_that('numeric profile', {
  num_rec <- okc_rec %>%
    step_profile(-age, profile = vars(age)) %>%
    prep(okc, retain = TRUE) %>%
    juice()
  expect_true(is_unq(num_rec$diet))
  expect_true(is_unq(num_rec$height))
  expect_true(is_unq(num_rec$location))
  expect_true(is_unq(num_rec$date))
  expect_false(is_unq(num_rec$age))

})


test_that('factor profile', {
  fact_rec <- okc_rec %>%
    step_profile(-diet, profile = vars(diet)) %>%
    prep(okc, retain = TRUE) %>%
    juice()
  expect_false(is_unq(fact_rec$diet))
  expect_true(is_unq(fact_rec$height))
  expect_true(is_unq(fact_rec$location))
  expect_true(is_unq(fact_rec$date))
  expect_true(is_unq(fact_rec$age))

})


test_that('date profile', {
  date_rec <- okc_rec %>%
    step_profile(-date, profile = vars(date)) %>%
    prep(okc, retain = TRUE) %>%
    juice()
  expect_true(is_unq(date_rec$diet))
  expect_true(is_unq(date_rec$height))
  expect_true(is_unq(date_rec$location))
  expect_false(is_unq(date_rec$date))
  expect_true(is_unq(date_rec$age))

})

test_that('character profile', {
  chr_rec <- okc_rec %>%
    step_profile(-location, profile = vars(location)) %>%
    prep(okc, retain = TRUE, stringsAsFactors = FALSE) %>%
    juice()
  expect_true(is_unq(chr_rec$diet))
  expect_true(is_unq(chr_rec$height))
  expect_false(is_unq(chr_rec$location))
  expect_true(is_unq(chr_rec$date))
  expect_true(is_unq(chr_rec$age))

})


test_that('bad values', {
  expect_error(
    okc_rec %>%
      step_profile(everything(), profile = vars(age)) %>%
      prep(data = okc)
  )
  expect_error(
    okc_rec %>%
      step_profile(everything(), profile = age) %>%
      prep(data = okc)
  )
  expect_error(
    okc_rec %>%
      step_profile(age, date, height, profile = vars(location, date)) %>%
      prep(data = okc)
  )
  expect_error(
    okc_rec %>%
      step_profile(starts_with("q"), profile = vars(age)) %>%
      prep(data = okc)
  )
  expect_error(
    okc_rec %>%
      step_profile(diet, profile = vars(age), pct = -1) %>%
      prep(data = okc)
  )
  expect_error(
    okc_rec %>%
      step_profile(diet, profile = vars(age), grid = 1:3) %>%
      prep(data = okc)
  )
  expect_error(
    okc_rec %>%
      step_profile(diet, profile = vars(age), grid = list(pctl = 1, len = 2)) %>%
      prep(data = okc)
  )
  expect_error(
    fixed(rep(c(TRUE, FALSE), each = 5))
  )

})

test_that('printing', {
  num_rec_1 <- okc_rec %>%
    step_profile(-age, profile = vars(age))
  num_rec_2 <- prep(num_rec_1, okc)

  expect_output(print(num_rec_1))
  expect_output(print(num_rec_2))
})



test_that('tidy', {
  num_rec_3 <- okc_rec %>%
    step_profile(-age, profile = vars(contains("age")))
  num_rec_4 <- prep(num_rec_3, okc)

  tidy_3 <- tidy(num_rec_3, 1)
  exp_3 <- tibble(
    terms = c("-age", "contains(\"age\")"),
    type = c("fixed", "profiled")
  )
  expect_equal(tidy_3, exp_3)

  tidy_4 <- tidy(num_rec_4, 1)
  exp_4 <- tibble(
    terms = c("diet", "height", "location", "date", "Class", "age"),
    type = c("fixed", "fixed", "fixed", "fixed", "fixed", "profiled")
  )
  expect_equal(tidy_4, exp_4)
})

