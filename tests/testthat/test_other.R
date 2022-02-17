library(testthat)
library(recipes)

library(modeldata)
data(okc)

set.seed(19)
in_test <- 1:200

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
  for (i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm = TRUE),
                 sum(okc_te$diet == i, na.rm = TRUE))

  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))

  location_props <- table(okc_tr$location)/sum(!is.na(okc_tr$location))
  location_props <- sort(location_props, decreasing = TRUE)
  location_levels <- names(location_props)[location_props >= others$step[[1]]$threshold]
  for (i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm = TRUE),
                 sum(okc_te$location == i, na.rm = TRUE))

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
  for (i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm = TRUE),
                 sum(okc_te$diet == i, na.rm = TRUE))

  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))

  location_props <- table(okc_tr$location)
  location_levels <- others$steps[[1]]$objects$location$keep
  for (i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm = TRUE),
                 sum(okc_te$location == i, na.rm = TRUE))

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

test_that('zero threshold - no removals', {
  others <- rec %>% step_other(diet, location, threshold = 0, other = "another")
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
  for (i in diet_levels)
    expect_equal(sum(others_te$diet == i, na.rm = TRUE),
                 sum(okc_te$diet == i, na.rm = TRUE))

  diet_levels <- c(diet_levels, others$step[[1]]$objects[["diet"]]$other)
  expect_true(all(levels(others_te$diet) %in% diet_levels))
  expect_true(all(diet_levels %in% levels(others_te$diet)))

  location_props <- table(okc_tr$location)/sum(!is.na(okc_tr$location))
  location_props <- sort(location_props, decreasing = TRUE)
  location_levels <- names(location_props)[location_props >= others$step[[1]]$threshold]
  for (i in location_levels)
    expect_equal(sum(others_te$location == i, na.rm = TRUE),
                 sum(okc_te$location == i, na.rm = TRUE))

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
           'A','C','C','B','A','B','C','D'),
    stringsAsFactors = FALSE)
  training <- df[1:10,]
  testing <- df[11:20,]
  training$y <- as.factor(training$y)
  training$x1 <- as.factor(training$x1)
  testing$y <- as.factor(testing$y)
  testing$x1 <- as.factor(testing$x1)

  novel_level <- recipe(y ~ ., data = training) %>%
    step_other(x1)

  novel_level <- prep(novel_level, training = training)
  new_results <- bake(novel_level, new_data = testing)
  orig_results <- bake(novel_level, new_data = training)
  expect_true(all(is.na(new_results$x1[testing$x1 == "C"])))
  expect_true(!any(orig_results$x1 == "other"))

  training <- df[1:14,]
  testing <- df[15:20,]
  training$y <- as.factor(training$y)
  training$x1 <- as.factor(training$x1)
  testing$y <- as.factor(testing$y)
  testing$x1 <- as.factor(testing$x1)

  novel_level <- recipe(y ~ ., data = training) %>%
    step_other(x1, threshold = .1)

  novel_level <- prep(novel_level, training = training)
  new_results <- bake(novel_level, new_data = testing)
  orig_results <- bake(novel_level, new_data = training)
  expect_true(all(new_results$x1[testing$x1 == "D"] == "other"))
  expect_true(any(new_results$x1 == "other"))
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

test_that(
  desc = "if threshold argument is an integer greater than one
          then it's treated as a frequency",
  code = {
    others <- rec %>% step_other(diet, location, threshold = 3000, other = "another", id = "")

    tidy_exp_un <- tibble(
      terms = c("diet", "location"),
      retained = rep(NA_character_, 2),
      id = ""
    )

    expect_equal(tidy_exp_un, tidy(others, number = 1))

    others <- prep(others, training = okc_tr)

    tidy_exp_tr <- tibble(
      terms = rep(c("diet", "location"), c(4, 3)),
      retained = c(
        "anything", "mostly anything", "mostly vegetarian",
        "strictly anything", "berkeley",
        "oakland", "san francisco"),
      id = ""
    )
    expect_equal(tidy_exp_tr, tidy(others, number = 1))
  }
)

test_that(
  desc = "if the threshold argument is greather than one then it should be
          an integer(ish)",
  code = {
    expect_error(rec %>% step_other(diet, location, threshold = 3.14))
  }
)

test_that(
  desc = "if threshold is equal to 1 then the function removes every factor
          level that is not present in the data",
  code = {
    fake_data <- data.frame(
      test_factor = factor(c("A", "B"), levels = c("A", "B", "C"))
    )

    rec <- recipe(~ test_factor, data = fake_data)
    others <- rec %>% step_other(test_factor, threshold = 1, id = "") %>% prep()

    tidy_exp_tr <- tibble(
      terms = rep("test_factor", 2),
      retained = c("A", "B"),
      id = ""
    )
    expect_equal(tidy_exp_tr, tidy(others, number = 1))
  }
)


test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_other(all_predictors())
  rec_param <- tunable.step_other(rec$steps[[1]])
  expect_equal(rec_param$name, c("threshold"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})


test_that('issue #415 -  strings to factor conversion', {
  trans_recipe <-
    recipe(Species ~ ., data = iris)

  prepped <- prep(trans_recipe, iris)

  iris_no_outcome <- iris
  iris_no_outcome["Species"] <- NULL

  expect_error(
    res <- bake(prepped, iris_no_outcome),
    regex = NA
  )
  expect_equal(names(res), names(iris[, 1:4]))
})


test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_other(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_other(rec)

  expect <- tibble(terms = character(), retained = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_other(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
