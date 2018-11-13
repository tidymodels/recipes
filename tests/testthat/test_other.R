library(testthat)
library(recipes)

context("Pooling to other category")


data(okc)

set.seed(19)
in_test <- sample(1:nrow(okc), size = 200)

okc_tr <- okc[-in_test,]
okc_te <- okc[ in_test,]

rec <- recipe(~ diet + location, data = okc_tr)

# assume no novel levels here but test later:
# all(sort(unique(okc_tr$location)) == sort(unique(okc$location)))

test_that('default inputs', {
  others <- rec %>% step_other(diet, location, other = "another", id = "")

  tidy_exp_un <- tibble(
    terms = c("diet", "location"),
    retained = rep(NA_character_, 2),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(others, number = 1))

  others <- prep(others, training = okc_tr)
  others_te <- bake(others, new_data = okc_te)

  tidy_exp_tr <- tibble(
    terms = rep(c("diet", "location"), c(4, 3)),
    retained = c(
      "anything", "mostly anything", "mostly vegetarian",
      "strictly anything", "berkeley",
      "oakland", "san francisco"),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(others, number = 1))

  diet_props <- table(okc_tr$diet)/sum(!is.na(okc_tr$diet))
  diet_props <- sort(diet_props, decreasing = TRUE)
  diet_levels <- names(diet_props)[diet_props >= others$step[[1]]$threshold]
  for(i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm =TRUE),
                 sum(okc_te$diet == i, na.rm =TRUE))

  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))

  location_props <- table(okc_tr$location)/sum(!is.na(okc_tr$location))
  location_props <- sort(location_props, decreasing = TRUE)
  location_levels <- names(location_props)[location_props >= others$step[[1]]$threshold]
  for(i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm =TRUE),
                 sum(okc_te$location == i, na.rm =TRUE))

  location_levels <- c(location_levels, others$step[[1]]$objects[["location"]]$other)
  expect_true(all(levels(others_te$location) %in% location_levels))
  expect_true(all(location_levels %in% levels(others_te$location)))

  expect_equal(is.na(okc_te$diet), is.na(others_te$diet))
  expect_equal(is.na(okc_te$location), is.na(others_te$location))
})


test_that('high threshold - much removals', {
  others <- rec %>% step_other(diet, location, threshold = .5)
  others <- prep(others, training = okc_tr)
  others_te <- bake(others, new_data = okc_te)

  diet_props <- table(okc_tr$diet)
  diet_levels <- others$steps[[1]]$objects$diet$keep
  for(i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm =TRUE),
                 sum(okc_te$diet == i, na.rm =TRUE))

  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))

  location_props <- table(okc_tr$location)
  location_levels <- others$steps[[1]]$objects$location$keep
  for(i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm =TRUE),
                 sum(okc_te$location == i, na.rm =TRUE))

  location_levels <- c(location_levels, others$step[[1]]$objects[["location"]]$other)
  expect_true(all(levels(others_te$location) %in% location_levels))
  expect_true(all(location_levels %in% levels(others_te$location)))

  expect_equal(is.na(okc_te$diet), is.na(others_te$diet))
  expect_equal(is.na(okc_te$location), is.na(others_te$location))
})


test_that('low threshold - no removals', {
  others <- rec %>% step_other(diet, location, threshold = 10^-30, other = "another")
  others <- prep(others, training = okc_tr, strings_as_factors = FALSE)
  others_te <- bake(others, new_data = okc_te)

  expect_equal(is.na(okc_te$diet), is.na(others_te$diet))
  expect_equal(is.na(okc_te$location), is.na(others_te$location))
  
  expect_equal(okc_te$diet, as.character(others_te$diet))
  expect_equal(okc_te$location, as.character(others_te$location))
})


test_that('factor inputs', {

  okc$diet <- as.factor(okc$diet)
  okc$location <- as.factor(okc$location)

  okc_tr <- okc[-in_test,]
  okc_te <- okc[ in_test,]

  rec <- recipe(~ diet + location, data = okc_tr)

  others <- rec %>% step_other(diet, location)
  others <- prep(others, training = okc_tr)
  others_te <- bake(others, new_data = okc_te)

  diet_props <- table(okc_tr$diet)/sum(!is.na(okc_tr$diet))
  diet_props <- sort(diet_props, decreasing = TRUE)
  diet_levels <- names(diet_props)[diet_props >= others$step[[1]]$threshold]
  for(i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm =TRUE),
                 sum(okc_te$diet == i, na.rm =TRUE))

  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))

  location_props <- table(okc_tr$location)/sum(!is.na(okc_tr$location))
  location_props <- sort(location_props, decreasing = TRUE)
  location_levels <- names(location_props)[location_props >= others$step[[1]]$threshold]
  for(i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm =TRUE),
                 sum(okc_te$location == i, na.rm =TRUE))

  location_levels <- c(location_levels, others$step[[1]]$objects[["location"]]$other)
  expect_true(all(levels(others_te$location) %in% location_levels))
  expect_true(all(location_levels %in% levels(others_te$location)))

  expect_equal(is.na(okc_te$diet), is.na(others_te$diet))
  expect_equal(is.na(okc_te$location), is.na(others_te$location))
})


test_that('novel levels', {
  df <- data.frame(
    y = c(1,0,1,1,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,0),
    x1 = c('A','B','B','B','B','A','A','A','B','A','A','B',
           'A','C','C','B','A','B','C','A'),
    stringsAsFactors = FALSE)
  training <- df[1:10,]
  testing <- df[11:20,]
  training$y <- as.factor(training$y)
  training$x1 <- as.factor(training$x1)
  testing$y <- as.factor(testing$y)
  testing$x1 <- as.factor(testing$x1)
  
  novel_level <- recipe(y ~ ., data = training) %>%
    step_other(x1) 
  
  novel_level <- prep(novel_level, training = training, retain = TRUE)
  new_results <- bake(novel_level, new_data = testing)
  orig_results <- bake(novel_level, new_data = training)
  expect_true(all(new_results$x1[testing$x1 == "C"] == "other"))
  expect_true(!any(orig_results$x1 == "other"))  
})

test_that("'other' already in use", {
  others <- rec %>% step_other(diet, location, threshold = 10^-10)
  expect_error(
    prep(others, training = okc_tr, strings_as_factors = FALSE)
  )
})

test_that('printing', {
  rec <- rec %>% step_other(diet, location)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_tr, verbose = TRUE))
})

