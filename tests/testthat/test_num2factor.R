library(recipes)
library(testthat)

n <- 200

set.seed(8575)
ex_dat <- data.frame(
  w = sample(letters[1:3], size = n, replace = TRUE),
  x = rnorm(n),
  z = rep(1:10, each = 20)
)

rec <- recipe(~ ., data = ex_dat)

test_that('basic functionality', {
  ex_1 <- rec %>%
    step_num2factor(all_numeric()) %>%
    prep(ex_dat, retain = TRUE) %>%
    juice
  expect_equal(class(ex_1$w), "factor")
  expect_equal(class(ex_1$x), "factor")
  expect_equal(class(ex_1$z), "factor")  
  expect_equal(levels(ex_1$z), sort(paste(1:10)))
  expect_equal(levels(ex_1$x), sort(paste(unique(ex_dat$x))))

  ex_2 <- rec %>%
    step_num2factor(all_numeric(), ordered = TRUE) %>%
    prep(ex_dat, retain = TRUE) %>%
    juice
  expect_equal(class(ex_2$x), c("ordered", "factor"))
  expect_equal(class(ex_2$x), c("ordered", "factor"))
  expect_equal(levels(ex_2$z), sort(paste(1:10)))
  expect_equal(levels(ex_2$x), sort(paste(unique(ex_dat$x))))
})

test_that('bad args', {
  expect_error(
  rec %>%
    step_num2factor(w, x) %>%
    prep(ex_dat)
  )
  expect_error(
    rec %>%
      step_string2factor(w) %>%
      prep(ex_dat)
  )
  expect_error(
    rec %>%
      step_string2factor(w, z, ordered = "yes") %>%
      prep(ex_dat)
  )
})


test_that('printing', {
  ex_3 <- rec %>%
    step_num2factor(all_numeric()) %>%
    prep(ex_dat, stringsAsFactors = FALSE, retain = TRUE)
  expect_output(print(ex_3))
  expect_output(prep(ex_3, training = ex_dat, verbose = TRUE))
})


