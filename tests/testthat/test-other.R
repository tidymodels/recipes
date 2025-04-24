library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")

set.seed(19)
in_test <- 1:200

sacr_tr <- Sacramento[-in_test, ]
sacr_te <- Sacramento[in_test, ]

rec <- recipe(~ city + zip, data = sacr_tr)

# assume no novel levels here but test later:
# all(sort(unique(sacr_tr$zip)) == sort(unique(Sacramento$zip)))

test_that("default inputs", {
  others <- rec |> step_other(city, zip, other = "another", id = "")

  tidy_exp_un <- tibble(
    terms = c("city", "zip"),
    retained = rep(NA_character_, 2),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(others, number = 1))

  others <- prep(others, training = sacr_tr)
  others_te <- bake(others, new_data = sacr_te)

  tidy_exp_tr <- tibble(
    terms = rep(c("city", "zip"), c(3, 1)),
    retained = c("ELK_GROVE", "ROSEVILLE", "SACRAMENTO", "z95823"),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(others, number = 1))

  city_props <- table(sacr_tr$city) / sum(!is.na(sacr_tr$city))
  city_props <- sort(city_props, decreasing = TRUE)
  city_levels <- names(city_props)[city_props >= others$step[[1]]$threshold]
  for (i in city_levels) {
    expect_equal(
      sum(others_te$city == i, na.rm = TRUE),
      sum(sacr_te$city == i, na.rm = TRUE)
    )
  }

  city_levels <- c(city_levels, others$step[[1]]$objects[["city"]]$other)
  expect_true(all(levels(others_te$city) %in% city_levels))
  expect_true(all(city_levels %in% levels(others_te$city)))

  zip_props <- table(sacr_tr$zip) / sum(!is.na(sacr_tr$zip))
  zip_props <- sort(zip_props, decreasing = TRUE)
  zip_levels <- names(zip_props)[zip_props >= others$step[[1]]$threshold]
  for (i in zip_levels) {
    expect_equal(
      sum(others_te$zip == i, na.rm = TRUE),
      sum(sacr_te$zip == i, na.rm = TRUE)
    )
  }

  zip_levels <- c(zip_levels, others$step[[1]]$objects[["zip"]]$other)
  expect_true(all(levels(others_te$zip) %in% zip_levels))
  expect_true(all(zip_levels %in% levels(others_te$zip)))

  expect_equal(is.na(sacr_te$city), is.na(others_te$city))
  expect_equal(is.na(sacr_te$zip), is.na(others_te$zip))
})

test_that("high threshold - much removals", {
  others <- rec |> step_other(city, zip, threshold = .5)
  others <- prep(others, training = sacr_tr)
  others_te <- bake(others, new_data = sacr_te)

  city_props <- table(sacr_tr$city)
  city_levels <- others$steps[[1]]$objects$city$keep
  for (i in city_levels) {
    expect_equal(
      sum(others_te$city == i, na.rm = TRUE),
      sum(sacr_te$city == i, na.rm = TRUE)
    )
  }

  city_levels <- c(city_levels, others$step[[1]]$objects[["city"]]$other)
  expect_true(all(levels(others_te$city) %in% city_levels))
  expect_true(all(city_levels %in% levels(others_te$city)))

  zip_props <- table(sacr_tr$zip)
  zip_levels <- others$steps[[1]]$objects$zip$keep
  for (i in zip_levels) {
    expect_equal(
      sum(others_te$zip == i, na.rm = TRUE),
      sum(sacr_te$zip == i, na.rm = TRUE)
    )
  }

  zip_levels <- c(zip_levels, others$step[[1]]$objects[["zip"]]$other)
  expect_true(all(levels(others_te$zip) %in% zip_levels))
  expect_true(all(zip_levels %in% levels(others_te$zip)))

  expect_equal(is.na(sacr_te$city), is.na(others_te$city))
  expect_equal(is.na(sacr_te$zip), is.na(others_te$zip))
})

test_that("low threshold - no removals", {
  sacr_te_chr <- sacr_te

  others <- recipe(~ city + zip, data = sacr_tr, strings_as_factors = FALSE) |>
    step_other(city, zip, threshold = 10^-30, other = "another")
  others <- prep(others, training = sacr_te_chr)
  others_te <- bake(others, new_data = sacr_te_chr)

  expect_equal(is.na(sacr_te_chr$city), is.na(others_te$city))
  expect_equal(is.na(sacr_te_chr$zip), is.na(others_te$zip))

  expect_equal(as.character(sacr_te_chr$city), as.character(others_te$city))
  expect_equal(as.character(sacr_te_chr$zip), as.character(others_te$zip))
})

test_that("zero threshold - no removals", {
  sacr_te_chr <- sacr_te

  others <- recipe(~ city + zip, data = sacr_tr, strings_as_factors = FALSE) |>
    step_other(city, zip, threshold = 0, other = "another")
  others <- prep(others, training = sacr_te_chr)
  others_te <- bake(others, new_data = sacr_te_chr)

  expect_equal(is.na(sacr_te_chr$city), is.na(others_te$city))
  expect_equal(is.na(sacr_te_chr$zip), is.na(others_te$zip))

  expect_equal(as.character(sacr_te_chr$city), as.character(others_te$city))
  expect_equal(as.character(sacr_te_chr$zip), as.character(others_te$zip))
})

test_that("factor inputs", {
  Sacramento$city <- as.factor(Sacramento$city)
  Sacramento$zip <- as.factor(Sacramento$zip)

  sacr_tr <- Sacramento[-in_test, ]
  sacr_te <- Sacramento[in_test, ]

  rec <- recipe(~ city + zip, data = sacr_tr)

  others <- rec |> step_other(city, zip)
  others <- prep(others, training = sacr_tr)
  others_te <- bake(others, new_data = sacr_te)

  city_props <- table(sacr_tr$city) / sum(!is.na(sacr_tr$city))
  city_props <- sort(city_props, decreasing = TRUE)
  city_levels <- names(city_props)[city_props >= others$step[[1]]$threshold]
  for (i in city_levels) {
    expect_equal(
      sum(others_te$city == i, na.rm = TRUE),
      sum(sacr_te$city == i, na.rm = TRUE)
    )
  }

  city_levels <- c(city_levels, others$step[[1]]$objects[["city"]]$other)
  expect_true(all(levels(others_te$city) %in% city_levels))
  expect_true(all(city_levels %in% levels(others_te$city)))

  zip_props <- table(sacr_tr$zip) / sum(!is.na(sacr_tr$zip))
  zip_props <- sort(zip_props, decreasing = TRUE)
  zip_levels <- names(zip_props)[zip_props >= others$step[[1]]$threshold]
  for (i in zip_levels) {
    expect_equal(
      sum(others_te$zip == i, na.rm = TRUE),
      sum(sacr_te$zip == i, na.rm = TRUE)
    )
  }

  zip_levels <- c(zip_levels, others$step[[1]]$objects[["zip"]]$other)
  expect_true(all(levels(others_te$zip) %in% zip_levels))
  expect_true(all(zip_levels %in% levels(others_te$zip)))

  expect_equal(is.na(sacr_te$city), is.na(others_te$city))
  expect_equal(is.na(sacr_te$zip), is.na(others_te$zip))
})

test_that("novel levels", {
  df <- data.frame(
    y = c(1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0),
    x1 = c(
      "A",
      "B",
      "B",
      "B",
      "B",
      "A",
      "A",
      "A",
      "B",
      "A",
      "A",
      "B",
      "A",
      "C",
      "C",
      "B",
      "A",
      "B",
      "C",
      "D"
    ),
    stringsAsFactors = FALSE
  )
  training <- df[1:10, ]
  testing <- df[11:20, ]
  training$y <- as.factor(training$y)
  training$x1 <- as.factor(training$x1)
  testing$y <- as.factor(testing$y)
  testing$x1 <- as.factor(testing$x1)

  novel_level <- recipe(y ~ ., data = training) |>
    step_other(x1)

  novel_level <- prep(novel_level, training = training)
  new_results <- bake(novel_level, new_data = testing)
  orig_results <- bake(novel_level, new_data = training)
  expect_true(all(is.na(new_results$x1[testing$x1 == "C"])))
  expect_true(!any(orig_results$x1 == "other"))

  training <- df[1:14, ]
  testing <- df[15:20, ]
  training$y <- as.factor(training$y)
  training$x1 <- as.factor(training$x1)
  testing$y <- as.factor(testing$y)
  testing$x1 <- as.factor(testing$x1)

  novel_level <- recipe(y ~ ., data = training) |>
    step_other(x1, threshold = .1)

  novel_level <- prep(novel_level, training = training)
  new_results <- bake(novel_level, new_data = testing)
  orig_results <- bake(novel_level, new_data = training)
  expect_true(all(new_results$x1[testing$x1 == "D"] == "other"))
  expect_true(any(new_results$x1 == "other"))
})

test_that("'other' already in use", {
  sacr_tr_chr <- sacr_tr |>
    dplyr::mutate(
      city = as.character(city),
      zip = as.character(zip),
      type = as.character(type)
    )

  sacr_tr_chr$city[1] <- "other"

  rec <- recipe(~ city + zip, data = sacr_tr_chr, strings_as_factors = FALSE)

  others <- rec |> step_other(city, zip, threshold = 10^-10)
  expect_snapshot(
    error = TRUE,
    prep(others, training = sacr_tr_chr)
  )
})

test_that(desc = "if threshold argument is an integer greater than one
          then it's treated as a frequency", code = {
  others <- rec |>
    step_other(city, zip, threshold = 80, other = "another", id = "")

  tidy_exp_un <- tibble(
    terms = c("city", "zip"),
    retained = rep(NA_character_, 2),
    id = ""
  )

  expect_equal(tidy_exp_un, tidy(others, number = 1))

  others <- prep(others, training = sacr_tr)

  tidy_exp_tr <- tibble(
    terms = c("city", "zip"),
    retained = c("SACRAMENTO", "z95823"),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(others, number = 1))
})

test_that(desc = "if the threshold argument is greather than one then it should be an integer(ish)", code = {
  expect_snapshot(
    error = TRUE,
    rec |> step_other(city, zip, threshold = 3.14) |> prep()
  )
})

test_that(desc = "bad values of threshold are treated correctly", code = {
  expect_snapshot(
    error = TRUE,
    rec |> step_other(city, zip, threshold = letters) |> prep()
  )
})

test_that(desc = "if threshold is equal to 1 then the function removes every factor
          level that is not present in the data", code = {
  fake_data <- data.frame(
    test_factor = factor(c("A", "B"), levels = c("A", "B", "C"))
  )

  rec <- recipe(~test_factor, data = fake_data)
  others <- rec |>
    step_other(test_factor, threshold = 1, id = "") |>
    prep()

  tidy_exp_tr <- tibble(
    terms = rep("test_factor", 2),
    retained = c("A", "B"),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(others, number = 1))
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_other(all_predictors())
  rec_param <- tunable.step_other(rec$steps[[1]])
  expect_equal(rec_param$name, c("threshold"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("issue #415 -  strings to factor conversion", {
  trans_recipe <-
    recipe(Species ~ ., data = iris)

  prepped <- prep(trans_recipe, iris)

  iris_no_outcome <- iris
  iris_no_outcome["Species"] <- NULL

  expect_no_error(
    res <- bake(prepped, iris_no_outcome)
  )
  expect_equal(names(res), names(iris[, 1:4]))
})

test_that("othering with case weights", {
  weighted_props <- sacr_tr |>
    mutate(sqft = as.double(sqft)) |>
    count(city, wt = sqft, sort = TRUE) |>
    mutate(prop = n / sum(n))
  sacr_tr_caseweights <- sacr_tr |>
    mutate(sqft = frequency_weights(sqft))

  for (n_cols in 1:5) {
    others <- recipe(~ city + sqft, data = sacr_tr_caseweights) |>
      step_other(
        city,
        other = "another",
        id = "",
        threshold = weighted_props$prop[n_cols]
      )

    others <- prep(others, training = sacr_tr_caseweights)
    expect_equal(n_cols, nrow(tidy(others, number = 1)))
  }

  expect_snapshot(others)

  # ----------------------------------------------------------------------------

  unweighted_props <- sacr_tr |>
    count(city, sort = TRUE) |>
    mutate(prop = n / sum(n))
  sacr_tr_caseweights <- sacr_tr |>
    mutate(sqft = importance_weights(sqft))

  for (n_cols in 1:5) {
    others <- recipe(~ city + sqft, data = sacr_tr_caseweights) |>
      step_other(
        city,
        other = "another",
        id = "",
        threshold = unweighted_props$prop[n_cols]
      )

    others <- prep(others, training = sacr_tr_caseweights)
    expect_equal(n_cols, nrow(tidy(others, number = 1)))
  }

  expect_snapshot(others)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  others <- rec |>
    step_other(city, zip, other = "another", id = "") |>
    update_role(city, zip, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  tidy_exp_un <- tibble(
    terms = c("city", "zip"),
    retained = rep(NA_character_, 2),
    id = ""
  )

  others <- prep(others, training = sacr_tr)

  expect_snapshot(error = TRUE, bake(others, new_data = sacr_te[, 3:9]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_other(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
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

  expect <- tibble(
    terms = character(),
    retained = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~ city + zip, data = sacr_tr) |>
    step_other(city, zip)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_other(
      all_predictors(),
      threshold = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_other(Species) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
