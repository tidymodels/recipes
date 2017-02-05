library(testthat)
library(recipes)
library(tibble)
library(dplyr)

items <- tibble(variable = colors())

test_that('simple formula', {
  f1 <- ~ dimgrey + yellow + navajowhite + navyblue 
  f1_exp <- c("dimgrey", "yellow", "navajowhite", "navyblue")
  f1_res <- recipes:::parse_terms_formula(f1, items)
  expect_equal(sort(f1_exp), sort(f1_res))
})

test_that('simple function', {
  f2 <- ~ contains("red") 
  f2_exp <- grep("red", items$variable, value = TRUE)
  f2_res <- recipes:::parse_terms_formula(f2, items)
  expect_equal(sort(f2_exp), sort(f2_res))
})

test_that('long simple formula', {
  f3 <- as.formula(paste("~", paste0(items$variable[1:200], collapse = "+")))
  f3_exp <- items$variable[1:200]
  ## Note: if we use the entire set of items$variable, `f_calls` triggers
  ## "during wrapup: evaluation nested too deeply: infinite recursion / 
  ## options(expressions=)?". Ask Hadley about that
  f3_res <- recipes:::parse_terms_formula(f3, items)
  expect_equal(sort(f3_exp), sort(f3_res))
})

test_that('composite formula', {
  f4 <- ~ ends_with("2") + yellowgreen
  f4_exp <- c("yellowgreen", grep("2$", items$variable, value = TRUE))
  f4_res <- recipes:::parse_terms_formula(f4, items)
  expect_equal(sort(f4_exp), sort(f4_res))
})

test_that('improper function', {
  expect_error(recipes:::parse_terms_formula(f~ log(yellow) + red, items))
})

test_that('simple minus', {
  f5 <- ~ ends_with("green") - yellowgreen
  f5_exp <- grep("green$", colors(), value = TRUE)
  f5_exp <- f5_exp[f5_exp != "yellowgreen"]
  f5_res <- recipes:::parse_terms_formula(f5, items)
  expect_equal(sort(f5_exp), sort(f5_res))
})

test_that('more complex minus', {
  f6 <- ~ ends_with("green") - yellowgreen + contains("red") - ends_with("red")
  f6_exp <- grep("(green$)|(red)", colors(), value = TRUE)
  f6_exp <- f6_exp[f6_exp != "yellowgreen"]
  f6_exp <- f6_exp[!grepl("red$", f6_exp)]
  f6_res <- recipes:::parse_terms_formula(f6, items)
  expect_equal(sort(f6_exp), sort(f6_res))
})

