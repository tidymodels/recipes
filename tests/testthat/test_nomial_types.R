library(recipes)
library(dplyr)
library(testthat)

context("Checking factors in new data versus original")

# ----------------------------------------------------------------

data("okc")

okc_chr <- 
  okc %>%
  mutate(Class = as.character(Class))

okc_fac <- 
  okc %>%
  mutate(diet = as.factor(diet))

okc_all_fac <- 
  okc_fac %>%
  mutate(location = as.factor(location)) 

# ----------------------------------------------------------------

test_that('factors all the way down', {
  tr <- 
    okc_all_fac %>%
    slice(1:500)
  
  te <- 
    okc_all_fac %>%
    slice(501:1000)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


test_that('factors all the way down with skipping', {
  tr <- 
    okc_all_fac %>%
    slice(1:500)
  
  te <- 
    okc_all_fac %>%
    slice(501:1000) %>%
    select(-Class)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_silent(check_nominal_type(te, rec$orig_lvls))
})

# ----------------------------------------------------------------

test_that('mixed nominal data', {
  tr <- 
    okc_fac %>%
    slice(1:500)
  te <- 
    okc_fac %>%
    slice(501:1000)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


test_that('mixed nominal data with skipping', {
  tr <- 
    okc_fac %>%
    slice(1:500)
  te <- 
    okc_fac %>%
    slice(501:1000) %>%
    select(-Class)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


# ----------------------------------------------------------------

test_that('no factors', {
  tr <- 
    okc_chr %>%
    slice(1:500)
  te <- 
    okc_chr %>%
    slice(501:1000)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


test_that('no factors with skipping', {
  tr <- 
    okc_chr %>%
    slice(1:500)
  te <- 
    okc_chr %>%
    slice(501:1000) %>%
    select(-Class)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_silent(check_nominal_type(te, rec$orig_lvls))
})


# ----------------------------------------------------------------

test_that('missing factors', {
  tr <- 
    okc_fac %>%
    slice(1:500)
  te <- 
    okc_chr %>%
    slice(501:1000)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_warning(check_nominal_type(te, rec$orig_lvls))
})

test_that('missing factors with skipping', {
  tr <- 
    okc_fac %>%
    slice(1:500)
  te <- 
    okc_chr %>%
    slice(501:1000) %>%
    select(-Class)
  
  rec <- 
    recipe(Class ~ ., data = tr) %>%
    prep(training = tr, retain = TRUE)
  
  expect_warning(check_nominal_type(te, rec$orig_lvls))
})
