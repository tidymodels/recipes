library(testthat)
library(magrittr)
library(recipes)

data(covers)
rec <- recipe(~ description, covers) %>%
  step_regex(description, pattern = "(rock|stony)", result = "rocks") %>%
  step_regex(description, pattern = "(rock|stony)", result = "more_rocks") 

test_that('default options', {
  rec1 <- rec %>% step_bin2factor(rocks)
  rec1 <- learn(rec1, training = covers)
  res1 <- process(rec1, newdata = covers)
  expect_true(all(diag(table(res1$rocks, res1$more_rocks)) == 0))
})


test_that('nondefault options', {
  rec2 <- rec %>% step_bin2factor(rocks, levels = letters[2:1])
  rec2 <- learn(rec2, training = covers)
  res2 <- process(rec2, newdata = covers)
  expect_true(all(diag(table(res2$rocks, res2$more_rocks)) == 0))
})


test_that('bad options', {
  rec3 <- rec %>% step_bin2factor(description)
  expect_error(learn(rec3, training = covers))
  expect_error(rec %>% step_bin2factor(rocks, levels = letters[1:5]))
  expect_error(rec %>% step_bin2factor(rocks, levels = 1:2))
})
