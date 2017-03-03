library(testthat)
library(magrittr)
library(recipes)

data(okc)

set.seed(19)
in_train <- sample(1:nrow(okc), size = 30000)

okc_tr <- okc[ in_train,]
okc_te <- okc[-in_train,]

rec <- recipe(~ diet + location, data = okc_tr)

test_that('default inputs', {
  others <- rec %>% step_other(~ diet  + location)
  others <- learn(others, training = okc_tr)
  others_te <- process(others, newdata = okc_te)
  
  diet_props <- table(okc_tr$diet)/sum(!is.na(okc_tr$diet))
  diet_props <- sort(diet_props, decreasing = TRUE)
  diet_levels <- names(diet_props)[cumsum(diet_props) <= others$step[[1]]$threshold]
  for(i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm =TRUE), 
                 sum(okc_te$diet == i, na.rm =TRUE))
  
  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))
  
  location_props <- table(okc_tr$location)/sum(!is.na(okc_tr$location))
  location_props <- sort(location_props, decreasing = TRUE)
  location_levels <- names(location_props)[cumsum(location_props) <= others$step[[1]]$threshold]
  for(i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm =TRUE), 
                 sum(okc_te$location == i, na.rm =TRUE))
  
  location_levels <- c(location_levels, others$step[[1]]$objects[["location"]]$other)
  expect_true(all(levels(others_te$location) %in% location_levels))
  expect_true(all(location_levels %in% levels(others_te$location)))

  expect_equal(is.na(okc_te$diet), is.na(others_te$diet))
  expect_equal(is.na(okc_te$location), is.na(others_te$location))
})


test_that('small threshold', {
  others <- rec %>% step_other(~ diet  + location, threshold = .1)
  others <- learn(others, training = okc_tr)
  others_te <- process(others, newdata = okc_te)
  
  diet_props <- table(okc_tr$diet)
  diet_levels <- names(diet_props)[which.max(diet_props)]
  for(i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm =TRUE), 
                 sum(okc_te$diet == i, na.rm =TRUE))
  
  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))
  
  location_props <- table(okc_tr$location)
  location_levels <- names(location_props)[which.max(location_props)]
  for(i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm =TRUE), 
                 sum(okc_te$location == i, na.rm =TRUE))
  
  location_levels <- c(location_levels, others$step[[1]]$objects[["location"]]$other)
  expect_true(all(levels(others_te$location) %in% location_levels))
  expect_true(all(location_levels %in% levels(others_te$location)))
  
  location_levels <- c(location_levels, others$step[[1]]$objects[["location"]]$other)
  expect_true(all(levels(others_te$location) %in% location_levels))
  expect_true(all(location_levels %in% levels(others_te$location)))
  
  expect_equal(is.na(okc_te$diet), is.na(others_te$diet))
  expect_equal(is.na(okc_te$location), is.na(others_te$location))
})


test_that('high threshold', {
  others <- rec %>% step_other(~ diet  + location, threshold = .99999999999)
  others <- learn(others, training = okc_tr)
  others_te <- process(others, newdata = okc_te)
  
  expect_equal(okc_te$diet, others_te$diet)
  expect_equal(okc_te$location, others_te$location)
})


test_that('factor inputs', {
  
  okc$diet <- as.factor(okc$diet)
  okc$location <- as.factor(okc$location)
  
  okc_tr <- okc[ in_train,]
  okc_te <- okc[-in_train,]
  
  rec <- recipe(~ diet + location, data = okc_tr)
  
  others <- rec %>% step_other(~ diet  + location)
  others <- learn(others, training = okc_tr)
  others_te <- process(others, newdata = okc_te)
  
  diet_props <- table(okc_tr$diet)/sum(!is.na(okc_tr$diet))
  diet_props <- sort(diet_props, decreasing = TRUE)
  diet_levels <- names(diet_props)[cumsum(diet_props) <= others$step[[1]]$threshold]
  for(i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm =TRUE), 
                 sum(okc_te$diet == i, na.rm =TRUE))
  
  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))
  
  location_props <- table(okc_tr$location)/sum(!is.na(okc_tr$location))
  location_props <- sort(location_props, decreasing = TRUE)
  location_levels <- names(location_props)[cumsum(location_props) <= others$step[[1]]$threshold]
  for(i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm =TRUE), 
                 sum(okc_te$location == i, na.rm =TRUE))
  
  location_levels <- c(location_levels, others$step[[1]]$objects[["location"]]$other)
  expect_true(all(levels(others_te$location) %in% location_levels))
  expect_true(all(location_levels %in% levels(others_te$location)))
  
  expect_equal(is.na(okc_te$diet), is.na(others_te$diet))
  expect_equal(is.na(okc_te$location), is.na(others_te$location))
})
