test_that("`juice()` returns a 0 column / N row tibble when a selection returns no columns", {
  rec <- recipe(~ ., data = iris)
  rec <- prep(rec, iris)

  expect_equal(
    juice(rec, all_outcomes()),
    tibble(.rows = nrow(iris))
  )
})
