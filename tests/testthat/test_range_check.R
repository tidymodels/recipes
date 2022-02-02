library(testthat)

x = -10:110

test_that("core function - correct input", {
  expect_error(range_check_func(x, -10, 110), NA)
  expect_error(range_check_func(as.character(x), -10, 110))
  expect_error(range_check_func(x, -10, 110, "a"))
  expect_error(range_check_func(x, -10, 110, .05), NA)
  expect_error(range_check_func(x, -10, 110, c(.05, .08)), NA)
  expect_error(range_check_func(x, -10, 110, c(.05, .08, .05)),
               "slack_prop should be of length 1 or of length 2")
})

test_that("core function - workings", {
  expect_error(range_check_func(x, -5, 110), NA)
  expect_error(range_check_func(x, 0, 100),
               "min x is -10, lower bound is -5, max x is 110, upper bound is 105")
  expect_error(range_check_func(x, 0, 110),
               "min x is -10, lower bound is -5.5")
  expect_error(range_check_func(x, -5, 100),
               "max x is 110, upper bound is 105.25")
  expect_error(range_check_func(x, 0, 100, slack_prop = c(0.05, 0.1)),
               "min x is -10, lower bound is -5")
  expect_error(range_check_func(x, 0, 100, slack_prop = c(0.1, 0.05)),
               "max x is 110, upper bound is 105")
  expect_warning(range_check_func(x, 0, 100, warn = TRUE),
                "min x is -10, lower bound is -5, max x is 110, upper bound is 105")
})

test_that("in recipe", {
  train <- tibble(x = c(0, 100), y = c(0, 50))
  test  <- tibble(x = c(-10, 110), y = c(-10, 60))
  rec1  <- recipe(train) %>% check_range(x, y, slack_prop = 0.2) %>% prep()
  expect_error(bake(rec1, test), NA)
  expect_warning(bake(rec1, test), NA)

  skip_if(packageVersion("rlang") < "1.0.0")
  rec2 <- recipe(train) %>% check_range(x, y) %>% prep()
  expect_snapshot(error = TRUE, bake(rec2, test))

  rec3 <- recipe(train) %>% check_range(x, y, warn = TRUE) %>% prep()
  expect_snapshot(bake(rec3, test))

  rec4 <- recipe(train) %>% check_range(y, slack_prop = c(0.2, 0.1)) %>% prep()
  expect_snapshot(error = TRUE, bake(rec4, test))
})

test_that('printing', {
  check_range_extract <- recipe(mtcars) %>%
    check_range(drat, cyl, am)
  expect_output(print(check_range_extract))
  expect_output(prep(check_range_extract, training = mtcars, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- check_range(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_range(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- check_range(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
