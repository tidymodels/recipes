library(recipes)
library(testthat)


test_that('simple skip', {
  rec_1 <- recipe(Sepal.Length ~ ., data = iris) %>%
    step_log(Sepal.Length, skip = TRUE) %>%
    step_dummy(Species) %>%
    step_center(all_predictors())
  
  prepped_1 <- prep(rec_1, training = iris, retain = TRUE)
  
  juiced_1 <- juice(prepped_1)
  baked_1  <- bake(prepped_1, newdata = iris)

  expect_equal(baked_1$Sepal.Length, iris$Sepal.Length)
  expect_equal(juiced_1$Sepal.Length, log(iris$Sepal.Length))

  prepped_2 <- prep(rec_1, training = iris, retain = FALSE)
  
  baked_2  <- bake(prepped_2, newdata = iris[, -1])
  baked_3  <- bake(prepped_2, newdata = iris)
  expect_false(
    isTRUE(
      all.equal(juiced_1$Sepal.Width, baked_3$Sepal.Length)
    )
  )
  expect_equal(log(baked_1$Sepal.Length), juiced_1$Sepal.Length)
  expect_equal(setdiff(names(baked_1), names(baked_2)), "Sepal.Length")
  
  expect_warning(prep(rec_1, training = iris, retain = FALSE))
})
