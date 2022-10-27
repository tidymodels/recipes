library(testthat)
library(recipes)
library(lubridate)
library(tibble)

examples <- data.frame(
  Dan = ymd("2002-03-04") + days(1:10),
  Stefan = ymd("2006-01-13") + days(1:10)
)

examples$Dan <- as.POSIXct(examples$Dan)

date_rec <- recipe(~ Dan + Stefan, examples) %>%
  step_date(all_predictors())

feats <- c("year", "doy", "week", "decimal", "semester", "quarter", "dow", "month")

test_that("default option", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = feats)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  date_exp <- tibble(
    Dan = examples$Dan,
    Stefan = examples$Stefan,
    Dan_year = vec_cast(year(examples$Dan), integer()),
    Dan_doy = vec_cast(yday(examples$Dan), integer()),
    Dan_week = vec_cast(week(examples$Dan), integer()),
    Dan_decimal = decimal_date(examples$Dan),
    Dan_semester = vec_cast(semester(examples$Dan), integer()),
    Dan_quarter = vec_cast(quarter(examples$Dan), integer()),
    Dan_dow = wday(examples$Dan, label = TRUE, abbr = TRUE),
    Dan_month = month(examples$Dan, label = TRUE, abbr = TRUE),
    Stefan_year = vec_cast(year(examples$Stefan), integer()),
    Stefan_doy = vec_cast(yday(examples$Stefan), integer()),
    Stefan_week = vec_cast(week(examples$Stefan), integer()),
    Stefan_decimal = decimal_date(examples$Stefan),
    Stefan_semester = vec_cast(semester(examples$Stefan), integer()),
    Stefan_quarter = vec_cast(quarter(examples$Stefan), integer()),
    Stefan_dow = wday(examples$Stefan, label = TRUE, abbr = TRUE),
    Stefan_month = month(examples$Stefan, label = TRUE, abbr = TRUE)
  )
  date_exp$Dan_dow <- factor(as.character(date_exp$Dan_dow), levels = levels(date_exp$Dan_dow))
  date_exp$Dan_month <- factor(as.character(date_exp$Dan_month), levels = levels(date_exp$Dan_month))
  date_exp$Stefan_dow <- factor(as.character(date_exp$Stefan_dow), levels = levels(date_exp$Stefan_dow))
  date_exp$Stefan_month <- factor(as.character(date_exp$Stefan_month), levels = levels(date_exp$Stefan_month))

  expect_identical(date_res, date_exp)
})


test_that("nondefault options", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = c("dow", "month"), label = FALSE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  date_exp <- tibble(
    Dan = examples$Dan,
    Stefan = examples$Stefan,
    Dan_dow = wday(examples$Dan, label = FALSE),
    Dan_month = month(examples$Dan, label = FALSE),
    Stefan_dow = wday(examples$Stefan, label = FALSE),
    Stefan_month = month(examples$Stefan, label = FALSE)
  )

  expect_equal(date_res, date_exp)
})


test_that("ordinal values", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = c("dow", "month"), ordinal = TRUE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  date_exp <- tibble(
    Dan = examples$Dan,
    Stefan = examples$Stefan,
    Dan_dow = wday(examples$Dan, label = TRUE),
    Dan_month = month(examples$Dan, label = TRUE),
    Stefan_dow = wday(examples$Stefan, label = TRUE),
    Stefan_month = month(examples$Stefan, label = TRUE)
  )

  expect_equal(date_res, date_exp)
})


test_that("printing", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = feats)
  expect_snapshot(print(date_rec))
  expect_snapshot(prep(date_rec))
})

test_that("keep_original_cols works", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = feats, keep_original_cols = FALSE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  expect_equal(
    colnames(date_res),
    c(paste0("Dan_", feats), paste0("Stefan_", feats))
  )
})

test_that("locale argument have recipe work in different locale", {
  old_locale <- Sys.getlocale("LC_TIME")
  withr::defer(Sys.setlocale("LC_TIME", old_locale))
  Sys.setlocale("LC_TIME", 'fr_FR.UTF-8')

  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors()) %>%
    prep()

  ref_res <- bake(date_rec, new_data = examples)

  Sys.setlocale("LC_TIME", old_locale)

  new_res <- bake(date_rec, new_data = examples)

  expect_equal(ref_res, new_res)
})

test_that("locale argument works when specified", {
  old_locale <- Sys.getlocale("LC_TIME")
  withr::defer(Sys.setlocale("LC_TIME", old_locale))
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors()) %>%
    prep()

  ref_res <- bake(date_rec, new_data = examples)

  Sys.setlocale("LC_TIME", 'fr_FR.UTF-8')

  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), locale = old_locale) %>%
    prep()

  new_res <- bake(date_rec, new_data = examples)

  expect_equal(ref_res, new_res)
})

test_that("locale argument works with clock locale", {
  labels_fr <- clock::clock_locale(labels = "fr")$labels

  date_rec <- recipe(~ Stefan, examples) %>%
    step_date(all_predictors(), locale = labels_fr, ordinal = TRUE) %>%
    prep()

  ref_res <- bake(date_rec, new_data = examples)

  expect_identical(
    ref_res$Stefan_dow,
    clock::date_weekday_factor(
      examples$Stefan,
      labels = labels_fr
    )
  )

  expect_identical(
    ref_res$Stefan_month,
    clock::date_month_factor(
      examples$Stefan,
      labels = labels_fr,
      abbreviate = TRUE
    )
  )
})

test_that("can bake and recipes with no locale", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors()) %>%
    prep()

  date_rec$steps[[1]]$locale <- NULL

  expect_error(
    date_res <- bake(date_rec, new_data = examples, all_predictors()),
    NA
  )
})

test_that("can prep recipes with no keep_original_cols", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = feats, keep_original_cols = FALSE)

  date_rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    date_rec <- prep(date_rec, training = examples, verbose = FALSE)
  )

  expect_error(
    date_res <- bake(date_rec, new_data = examples, all_predictors()),
    NA
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_date(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_date(rec)

  expect <- tibble(terms = character(), value = character(), ordinal = logical(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_date(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(Dan, features = feats) %>%
    update_role(Dan, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, new_data = examples)

  expect_error(bake(date_rec, new_data = examples[, 2, drop = FALSE]),
               class = "new_data_missing_column")
})
