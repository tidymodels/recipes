library(testthat)
library(recipes)

n <- 20

set.seed(752)
as_fact <- data.frame(
  numbers = rnorm(n),
  fact = factor(sample(letters[1:3], n, replace = TRUE)),
  ord = factor(sample(LETTERS[22:26], n, replace = TRUE),
               ordered = TRUE)
)
as_str <- as_fact
as_str$fact <- as.character(as_str$fact)
as_str$ord <- as.character(as_str$ord)

test_that('stringsAsFactors = FALSE', {
  rec1 <- recipe(~ ., data = as_fact) %>%
    step_center(numbers)
  rec1 <- prep(rec1, training = as_fact, retain = TRUE, 
                  stringsAsFactors = FALSE, verbose = FALSE)
  rec1_as_fact <- bake(rec1, newdata = as_fact)
  rec1_as_str <- bake(rec1, newdata = as_str) 
  expect_equal(as_fact$fact, rec1_as_fact$fact)
  expect_equal(as_fact$ord, rec1_as_fact$ord)  
  expect_equal(as_str$fact, rec1_as_str$fact)
  expect_equal(as_str$ord, rec1_as_str$ord)    
  
})

test_that('stringsAsFactors = TRUE', {
  rec2 <- recipe(~ ., data = as_fact) %>%
    step_center(numbers)
  rec2 <- prep(rec2, training = as_fact, retain = TRUE, 
                  stringsAsFactors = TRUE, verbose = FALSE)
  rec2_as_fact <- bake(rec2, newdata = as_fact)
  rec2_as_str <- bake(rec2, newdata = as_str) 
  expect_equal(as_fact$fact, rec2_as_fact$fact)
  expect_equal(as_fact$ord, rec2_as_fact$ord)  
  expect_equal(as_fact$fact, rec2_as_str$fact)
  expect_equal(as_fact$ord, rec2_as_str$ord)    
})
