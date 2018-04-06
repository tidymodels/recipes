library(testthat)
library(recipes)

data(covers)
rec <- recipe(~ description, covers) %>%
  step_regex(description, pattern = "(rock|stony)", result = "rocks") %>%
  step_regex(description, pattern = "(rock|stony)", result = "more_rocks")

test_that('default options', {
  rec1 <- rec %>% step_bin2factor(rocks)
  rec1 <- prep(rec1, training = covers)
  res1 <- bake(rec1, newdata = covers)
  expect_true(all(diag(table(res1$rocks, res1$more_rocks)) == 0))
})


test_that('nondefault options', {
  rec2 <- rec %>% step_bin2factor(rocks, levels = letters[2:1])
  rec2 <- prep(rec2, training = covers)
  res2 <- bake(rec2, newdata = covers)
  expect_true(all(diag(table(res2$rocks, res2$more_rocks)) == 0))
})


test_that('bad options', {
  rec3 <- rec %>% step_bin2factor(description)
  expect_error(prep(rec3, training = covers))
  expect_error(rec %>% step_bin2factor(rocks, levels = letters[1:5]))
  expect_error(rec %>% step_bin2factor(rocks, levels = 1:2))
})


test_that('printing', {
  rec2 <- rec %>% step_bin2factor(rocks, levels = letters[2:1])
  expect_output(print(rec2))
  expect_output(prep(rec2, training = covers, verbose = TRUE))
})


test_that('choose reference level', {
  rec4 <- rec %>% step_bin2factor(rocks, ref_first = FALSE)
  rec4 <- prep(rec4, training = covers)
  res4 <- bake(rec4, newdata = covers)
  expect_true(all(res4$rocks[res4$more_rocks == 1] == "yes"))
  expect_true(all(res4$rocks[res4$more_rocks == 0] == "no"))
  expect_true(levels(res4$rocks)[1] == "no")
})
