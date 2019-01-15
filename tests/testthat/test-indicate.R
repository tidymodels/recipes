context("test-indicate")

i <- iris
diag(i) <- NA

rec <- recipe(~ Sepal.Width + Sepal.Length, data = i) %>%
  step_indicate(Sepal.Width) %>%
  step_indicate(Sepal.Length, remove = FALSE) %>%
  prep()

juice(rec)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
