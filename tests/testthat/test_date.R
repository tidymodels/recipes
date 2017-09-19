library(testthat)
library(recipes)
library(lubridate)
library(tibble)

examples <- data.frame(Dan = ymd("2002-03-04") + days(1:10),
                       Stefan = ymd("2006-01-13") + days(1:10))

examples$Dan <- as.POSIXct(examples$Dan)

date_rec <- recipe(~ Dan + Stefan, examples) %>%
  step_date(all_predictors())

feats <- c("year", "doy", "week", "decimal", "semester", "quarter", "dow", "month")

test_that('default option', {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = feats)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, newdata = examples)

  date_exp <- tibble(
    Dan = examples$Dan,
    Stefan = examples$Stefan,
    Dan_year = year(examples$Dan),
    Dan_doy = yday(examples$Dan),
    Dan_week = week(examples$Dan),
    Dan_decimal = decimal_date(examples$Dan),
    Dan_semester = semester(examples$Dan),
    Dan_quarter = quarter(examples$Dan),
    Dan_dow = wday(examples$Dan, label = TRUE, abbr = TRUE),
    Dan_month = month(examples$Dan, label = TRUE, abbr = TRUE),
    Stefan_year = year(examples$Stefan),
    Stefan_doy = yday(examples$Stefan),
    Stefan_week = week(examples$Stefan),
    Stefan_decimal = decimal_date(examples$Stefan),
    Stefan_semester = semester(examples$Stefan),
    Stefan_quarter = quarter(examples$Stefan),
    Stefan_dow = wday(examples$Stefan, label = TRUE, abbr = TRUE),
    Stefan_month = month(examples$Stefan, label = TRUE, abbr = TRUE)
  )
  date_exp$Dan_dow <- factor(as.character(date_exp$Dan_dow), levels = levels(date_exp$Dan_dow))
  date_exp$Dan_month <- factor(as.character(date_exp$Dan_month), levels = levels(date_exp$Dan_month))
  date_exp$Stefan_dow <- factor(as.character(date_exp$Stefan_dow), levels = levels(date_exp$Stefan_dow))
  date_exp$Stefan_month <- factor(as.character(date_exp$Stefan_month), levels = levels(date_exp$Stefan_month))

  expect_equal(date_res, date_exp)
})


test_that('nondefault options', {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = c("dow", "month"), label = FALSE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, newdata = examples)

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


test_that('ordinal values', {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = c("dow", "month"), ordinal = TRUE)

  date_rec <- prep(date_rec, training = examples)
  date_res <- bake(date_rec, newdata = examples)

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


test_that('printing', {
  date_rec <- recipe(~ Dan + Stefan, examples) %>%
    step_date(all_predictors(), features = feats)
  expect_output(print(date_rec))
  expect_output(prep(date_rec, training = examples, verbose = TRUE))
})
