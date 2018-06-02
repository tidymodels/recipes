library(testthat)
library(recipes)

test_that('is trained?', {
  rec1 <- recipe(~ ., data = iris) 
  expect_true(fully_trained(rec1))
  
  rec2 <- rec1 %>%
    step_sqrt(all_numeric()) %>%
    step_center(all_numeric())
  expect_false(fully_trained(rec2))
  
  rec3 <- prep(rec2, training = iris, retain = TRUE)
  expect_true(fully_trained(rec3))
  
  rec4 <- rec3 %>% step_scale(all_numeric())
  expect_false(fully_trained(rec4))  
  
  rec5 <- prep(rec4, training = iris)
  expect_true(fully_trained(rec5))
  
})

test_that('formulas', {
  rec6 <- recipe(Species ~ ., data = iris)
  expect_equal(
    formula(rec6),
    as.formula(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width)
  )
  
  rec7 <- rec6 %>% step_rm(starts_with("Sepal")) %>% prep(iris)
  expect_equal(
    formula(rec7),
    as.formula(Species ~ Petal.Length + Petal.Width)
  )  
  
  rec8 <- recipe(~ ., data = iris)
  expect_equal(
    formula(rec8),
    as.formula( ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width + Species)
  )
  
  rec9 <- recipe(Species + Sepal.Length ~ ., data = iris)
  expect_equal(
    formula(rec9),
    as.formula(Species + Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width)
  )  
})

test_that('bad args', {
  
  rec10 <- recipe(Species ~ ., data = iris) %>%
    step_center(all_numeric())
  expect_error(formula(rec10))
})

