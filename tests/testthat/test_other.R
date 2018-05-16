library(testthat)
library(recipes)

data(okc)

set.seed(19)
in_train <- sample(1:nrow(okc), size = 30000)

okc_tr <- okc[ in_train,]
okc_te <- okc[-in_train,]

rec <- recipe(~ diet + location, data = okc_tr)

test_that('default inputs', {
  others <- rec %>% step_other(diet, location)

  tidy_exp_un <- tibble(
    terms = c("diet", "location"),
    retained = rep(NA_character_, 2)
  )
  expect_equal(tidy_exp_un, tidy(others, number = 1))

  others <- prep(others, training = okc_tr)
  others_te <- bake(others, newdata = okc_te)

  tidy_exp_tr <- tibble(
    terms = rep(c("diet", "location"), c(4, 3)),
    retained = c(
      "anything", "mostly anything", "mostly vegetarian",
      "strictly anything", "berkeley",
      "oakland", "san francisco")
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
  others_te <- bake(others, newdata = okc_te)

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
  others <- rec %>% step_other(diet, location, threshold = 10^-10)
  others <- prep(others, training = okc_tr, stringsAsFactors = FALSE)
  others_te <- bake(others, newdata = okc_te)

  expect_equal(others$steps[[1]]$objects$diet$collapse, FALSE)
  expect_equal(others$steps[[1]]$objects$location$collapse, FALSE)

  expect_equal(okc_te$diet, others_te$diet)
  expect_equal(okc_te$location, others_te$location)
})


test_that('factor inputs', {

  okc$diet <- as.factor(okc$diet)
  okc$location <- as.factor(okc$location)

  okc_tr <- okc[ in_train,]
  okc_te <- okc[-in_train,]

  rec <- recipe(~ diet + location, data = okc_tr)

  others <- rec %>% step_other(diet, location)
  others <- prep(others, training = okc_tr)
  others_te <- bake(others, newdata = okc_te)

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


test_that('printing', {
  rec <- rec %>% step_other(diet, location)
  expect_output(print(rec))
  expect_output(prep(rec, training = okc_tr, verbose = TRUE))
})

