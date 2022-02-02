library(testthat)
library(recipes)

library(modeldata)
data(covers)
covers$rows <- 1:nrow(covers)
covers$ch_rows <- paste(1:nrow(covers))

rec <- recipe(~ description + rows + ch_rows, covers)

test_that('default options', {
  rec1 <- rec %>%
    step_regex(description, pattern = "(rock|stony)") %>%
    step_regex(description, result = "all ones")
  rec1 <- prep(rec1, training = covers)
  res1 <- bake(rec1, new_data = covers)
  expect_equal(res1$X.rock.stony.,
               as.numeric(grepl("(rock|stony)", covers$description)))
  expect_equal(res1$`all ones`, rep(1, nrow(covers)))
})


test_that('nondefault options', {
  rec2 <- rec %>%
    step_regex(description, pattern = "(rock|stony)",
               result = "rocks",
               options = list(fixed = TRUE))
  rec2 <- prep(rec2, training = covers)
  res2 <- bake(rec2, new_data = covers)
  expect_equal(res2$rocks, rep(0, nrow(covers)))
})


test_that('bad selector(s)', {
  expect_error(rec %>% step_regex(description, rows, pattern = "(rock|stony)"))
  rec4 <- rec %>% step_regex(rows, pattern = "(rock|stony)")
  expect_error(prep(rec4, training = covers))
})


test_that('printing', {
  rec1 <- rec %>%
    step_regex(description, pattern = "(rock|stony)")
  expect_output(print(rec1))
  expect_output(prep(rec1, training = covers, verbose = TRUE))
})

test_that("empty selection prep/bake adds a 0 column", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_regex(rec1, pattern = "xxx")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked2$xxx, rep(0, nrow(mtcars)))
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_regex(rec)

  expect <- tibble(terms = character(), result = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_regex(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
