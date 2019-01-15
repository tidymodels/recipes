library(testthat)
library(recipes)

context("pattern counting")


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
  rec3 <- rec %>% step_count(starts_with("b"), pattern = "(rock|stony)")
  expect_error(prep(rec3, training = covers))
  rec4 <- rec %>% step_count(rows, pattern = "(rock|stony)")
  expect_error(prep(rec4, training = covers))
})


test_that('printing', {
  rec5 <- rec %>%
    step_count(description, pattern = "(rock|stony)")
  expect_output(print(rec5))
  expect_output(prep(rec5, training = covers, verbose = TRUE))
})
