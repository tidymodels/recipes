library(testthat)
library(recipes)
library(tibble)
library(dplyr)

## Some percentage of this is deprecated use of parse_terms_formula

set.seed(13)
items <- tibble(variable = colors(),
                type = sample(c("nominal", "numeric"), size = length(colors()), replace = TRUE),
                role = sample(c("predictor", "outcome"), size = length(colors()), replace = TRUE))

test_that('simple var formula', {
  f1 <- ~ dimgrey + yellow + navajowhite + navyblue 
  f1_exp <- c("dimgrey", "yellow", "navajowhite", "navyblue")
  f1_res <- parse_terms_formula(f1, items)
  expect_equal(sort(f1_exp), sort(f1_res))
})

test_that('simple var function', {
  f2 <- ~ contains("red") 
  f2_exp <- grep("red", items$variable, value = TRUE)
  f2_res <- parse_terms_formula(f2, items)
  expect_equal(sort(f2_exp), sort(f2_res))
})

test_that('long simple var formula', {
  f3 <- as.formula(paste("~", paste0(items$variable[1:200], collapse = "+")))
  f3_exp <- items$variable[1:200]
  ## Note: if we use the entire set of items$variable, `f_calls` triggers
  ## "during wrapup: evaluation nested too deeply: infinite recursion / 
  ## options(expressions=)?". Ask Hadley about that
  f3_res <- parse_terms_formula(f3, items)
  expect_equal(sort(f3_exp), sort(f3_res))
})

test_that('composite var formula', {
  f4 <- ~ ends_with("2") + yellowgreen
  f4_exp <- c("yellowgreen", grep("2$", items$variable, value = TRUE))
  f4_res <- parse_terms_formula(f4, items)
  expect_equal(sort(f4_exp), sort(f4_res))
})

test_that('improper var function', {
  expect_error(parse_terms_formula(f~ log(yellow) + red, items))
})

test_that('simple var minus', {
  f5 <- ~ ends_with("green") - yellowgreen
  f5_exp <- grep("green$", colors(), value = TRUE)
  f5_exp <- f5_exp[f5_exp != "yellowgreen"]
  f5_res <- parse_terms_formula(f5, items)
  expect_equal(sort(f5_exp), sort(f5_res))
})

test_that('more complex var minus', {
  f6 <- ~ ends_with("green") - yellowgreen + contains("red") - ends_with("red")
  f6_exp <- grep("(green$)|(red)", colors(), value = TRUE)
  f6_exp <- f6_exp[f6_exp != "yellowgreen"]
  f6_exp <- f6_exp[!grepl("red$", f6_exp)]
  f6_res <- parse_terms_formula(f6, items)
  expect_equal(sort(f6_exp), sort(f6_res))
})

test_that('simple role function', {
  f7 <- ~ has_role("predictor") 
  f7_exp <- items$variable[items$role == "predictor"]
  f7_res <- parse_terms_formula(f7, items)
  expect_equal(sort(f7_exp), sort(f7_res))
})

test_that('simple role function pt 2', {
  f8 <- ~ all_outcomes() 
  f8_exp <- items$variable[items$role == "outcome"]
  f8_res <- parse_terms_formula(f8, items)
  expect_equal(sort(f8_exp), sort(f8_res))
})

test_that('simple type function', {
  f9 <- ~ has_type("nominal") 
  f9_exp <- items$variable[items$type == "nominal"]
  f9_res <- parse_terms_formula(f9, items)
  expect_equal(sort(f9_exp), sort(f9_res))
})

test_that('simple type function pt 2', {
  f10 <- ~ all_numeric() 
  f10_exp <- items$variable[items$type == "numeric"]
  f10_res <- parse_terms_formula(f10, items)
  expect_equal(sort(f10_exp), sort(f10_res))
})

