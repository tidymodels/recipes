library(testthat)
library(recipes)
library(tibble)

set.seed(5522)
sim_dat <- data.frame(x1 = (20:100) / 10)
n <- nrow(sim_dat)
sim_dat$y1 <- sin(sim_dat$x1) + rnorm(n, sd = 0.1)
sim_dat$y2 <- cos(sim_dat$x1) + rnorm(n, sd = 0.1)
sim_dat$x2 <- runif(n)
sim_dat$x3 <- rnorm(n)
sim_dat$fac <- sample(letters[1:3], size = n, replace = TRUE)

rec <- recipe( ~ ., data = sim_dat)

test_that('error checks', {
  
  expect_error(rec %>% step_window())
  expect_error(rec %>% step_window(y1, size = 6))
  expect_error(rec %>% step_window(y1, size = NA))
  expect_error(rec %>% step_window(y1, size = NULL))
  expect_error(rec %>% step_window(y1, statistic = "average"))
  expect_error(rec %>% step_window(y1, size = 1)) 
  expect_error(rec %>% step_window(y1, size = 2)) 
  expect_error(rec %>% step_window(y1, size = -1))
  expect_warning(rec %>% step_window(y1, size = pi)) 
  expect_error(prep(rec %>% step_window(fac), training = sim_dat)) 
  expect_error(prep(rec %>% step_window(y1, size = 1000L), training = sim_dat))   
  bad_names <- rec %>%
    step_window(starts_with("y"), names = "only_one_name")
  expect_error(prep(bad_names, training = sim_dat))
  
})

test_that('basic moving average', {
  simple_ma <- rec %>%
    step_window(starts_with("y"))
  simple_ma <- prep(simple_ma, training = sim_dat)
  simple_ma_res <- bake(simple_ma, newdata = sim_dat)
  expect_equal(names(sim_dat), names(simple_ma_res))
  
  for (i in 2:(n - 1)) {
    expect_equal(simple_ma_res$y1[i], mean(sim_dat$y1[(i - 1):(i + 1)]))
    expect_equal(simple_ma_res$y2[i], mean(sim_dat$y2[(i - 1):(i + 1)]))
  }
  expect_equal(simple_ma_res$y1[1], mean(sim_dat$y1[1:3]))
  expect_equal(simple_ma_res$y2[1], mean(sim_dat$y2[1:3]))
  expect_equal(simple_ma_res$y1[n], mean(sim_dat$y1[(n - 2):n]))
  expect_equal(simple_ma_res$y2[n], mean(sim_dat$y2[(n - 2):n]))  

})

test_that('creating new variables', {
  new_names <- rec %>%
    step_window(starts_with("y"), names = paste0("new", 1:2), role = "predictor")
  new_names <- prep(new_names, training = sim_dat)
  new_names_res <- bake(new_names, newdata = sim_dat)
  
  simple_ma <- rec %>%
    step_window(starts_with("y"))
  simple_ma <- prep(simple_ma, training = sim_dat)
  simple_ma_res <- bake(simple_ma, newdata = sim_dat)
  
  expect_equal(new_names_res$new1, simple_ma_res$y1)
  expect_equal(new_names_res$new2, simple_ma_res$y2)  
})

test_that('printing', {
  new_names <- rec %>%
    step_window(starts_with("y"), names = paste0("new", 1:2), role = "predictor")
  expect_output(print(new_names))
  expect_output(prep(new_names, training = sim_dat))
})


