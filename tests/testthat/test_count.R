library(testthat)
library(recipes)

library(modeldata)
data(covers)
covers$rows <- 1:nrow(covers)
covers$ch_rows <- paste(1:nrow(covers))

rec <- recipe(~ description + rows + ch_rows, covers)

counts <- gregexpr(pattern = "(rock|stony)", text = covers$description)
counts <- vapply(counts, function(x) length(x[x>0]), integer(1))
chars <- nchar(covers$description)


test_that('default options', {
  rec1 <- rec %>%
    step_count(description, pattern = "(rock|stony)") %>%
    step_count(description, pattern = "", result = "every thing") %>%
    step_count(description, pattern = "(rock|stony)",
               result = "pct", normalize = TRUE)
  rec1 <- prep(rec1, training = covers)
  res1 <- bake(rec1, new_data = covers)
  expect_equal(res1$X.rock.stony., counts)
  expect_equal(res1$`every thing`, chars)
  expect_equal(res1$pct, counts/chars)
})


test_that('nondefault options', {
  rec2 <- rec %>%
    step_count(description, pattern = "(rock|stony)",
               result = "rocks",
               options = list(fixed = TRUE))
  rec2 <- prep(rec2, training = covers)
  res2 <- bake(rec2, new_data = covers)
  expect_equal(res2$rocks, rep(0, nrow(covers)))
})


test_that('bad selector(s)', {
  expect_error(rec %>% step_count(description, rows, pattern = "(rock|stony)"))
  rec2 <- rec %>% step_count(rows, pattern = "(rock|stony)")
  expect_error(prep(rec2, training = covers))
})


test_that('printing', {
  rec5 <- rec %>%
    step_count(description, pattern = "(rock|stony)")
  expect_output(print(rec5))
  expect_output(prep(rec5, training = covers, verbose = TRUE))
})

test_that("empty selection prep/bake adds an NA column", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_count(rec1, pattern = "rock")

  rec2 <- prep(rec2, mtcars)

  baked2 <- bake(rec2, mtcars)

  expect_identical(baked2$rock, rep(NA_integer_, nrow(mtcars)))
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_count(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), result = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), result = character(), id = character())
  )
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_count(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
