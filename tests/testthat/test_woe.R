library(tidyverse)
library(magrittr)

set.seed(1)
df <- data.frame(x1 = sample(c("A", "B", "C"), size = 20, replace = TRUE) %>% factor,
                 x2 = sample(c("A", "B", "C"), size = 20, replace = TRUE)) %>%
  mutate(y = rbinom(20, 1, prob = 1/(1 + exp(-1 * (-4 + as.numeric(x1) + as.numeric(x2)))))) %>%
  mutate(y = if_else(y == 1, "A", "B"))

#------------------------------------
context("woe_table")

test_that("woe_table do not accept different length inputs", {
  expect_error(woe_table(rep(c(0, 1), 20), rep(letters[1:4], 5)))
})

test_that("woe_table accepts only outcome with 2 distinct categories", {
  expect_error(woe_table(rep(c(0, 1, 2), 10), rep(letters[1:3], 10)))
  expect_error(woe_table(rep(c(0), 30), rep(letters[1:3], 10)))
  expect_error(woe_table(df$x1, df$x2))
})

test_that("woe_table returns a proper tibble", {
  expect_equal(dim(woe_table(df$y, df$x1)), c(3, 7))
  expect_identical(names(woe_table(df$y, df$x1)), c("predictor", "n_tot", "n_A", "n_B", "p_A", "p_B", "woe"))
})

test_that("logical outcome variables are treated properly", {
  expect_equal(dim(woe_table(c(TRUE, FALSE, TRUE, FALSE), c("A", "A", "A", "B"))), c(2, 7))
})

test_that("logical predictor variable are treated properly", {
  expect_equal(class(woe_table(c("A", "A", "A", "B"), c(TRUE, FALSE, TRUE, FALSE))$predictor), "character")
})

test_that("woe_table ruturns no messages nor warnings", {
  expect_silent(woe_table(c("A", "A", "A", "B"), c(TRUE, FALSE, TRUE, FALSE)))
  expect_silent(woe_table(df$y, df$x1))
})

test_that("odds_offset works", {
  expect_true(all(is.finite(woe_table(c(0, 0, 0, 1), c("A", "A", "B", "B"), odds_offset = 1e-6)$woe)))
  expect_false(all(is.finite(woe_table(c(0, 0, 0, 1), c("A", "A", "B", "B"), odds_offset = 0)$woe)))
})

