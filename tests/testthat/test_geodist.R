library(testthat)
library(recipes)

context("Geographic distances")

set.seed(4693)
rand_data <- data.frame(x = round(runif(10), 2), y = round(runif(10), 2))
rand_data$x[1] <- NA

dists <- 
  apply(as.matrix(rand_data),
        1,
        function(x, y, z)
          sqrt( (x[1] - y)^2 + (x[2] - z)^2 ),
        y = 0.5, z = 0.25)


test_that('basic functionality', {
  rec <- recipe( ~ x + y, data = rand_data) %>%
    step_geodist(x, y, ref_lat = 0.5, ref_lon = 0.25, log = FALSE)
  rec_trained <- prep(rec, traning = rand_data, retain = TRUE)
  
  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, rand_data, all_predictors())
  
  expect_equal(tr_int[["geo_dist"]], dists)
  expect_equal(te_int[["geo_dist"]], dists)
  
  rec_log <- recipe( ~ x + y, data = rand_data) %>%
    step_geodist(x, y, ref_lat = 0.5, ref_lon = 0.25, log = TRUE)
  rec_log_trained <- prep(rec_log, traning = rand_data, retain = TRUE)
  
  tr_log_int <- juice(rec_log_trained, all_predictors())
  te_log_int <- bake(rec_log_trained, rand_data, all_predictors())
  
  expect_equal(tr_log_int[["geo_dist"]], log(dists))
  expect_equal(te_log_int[["geo_dist"]], log(dists))
  
})

test_that('bad args', {
  rand_data_2 <- rand_data
  rand_data_2$x1 <- runif(nrow(rand_data_2))
  rand_data_2$y1 <- runif(nrow(rand_data_2))
  rec <- recipe( ~ ., data = rand_data_2)
  
  expect_error(
    rec %>% 
      step_geodist(starts_with("x"), y, ref_lat = 0.5, ref_lon = 0.25) %>%
      prep(training = rand_data_2)
  )
  expect_error(
    rec %>% 
      step_geodist(x, starts_with("y"), ref_lat = 0.5, ref_lon = 0.25) %>%
      prep(training = rand_data_2)
  )
  expect_error(
    rec %>% 
      step_geodist(x, y, ref_lat = letters[1:2], ref_lon = 0.25) %>%
      prep(training = rand_data_2)
  )
  expect_error(
    rec %>% 
      step_geodist(x, y, ref_lon = letters[1:2], ref_lat = 0.25) %>%
      prep(training = rand_data_2)
  )  
  expect_error(
    rec %>% 
      step_geodist(x, y, ref_lon = 0.5, ref_lat = 0.25, name = 1) %>%
      prep(training = rand_data_2)
  ) 
  expect_error(
    rec %>% 
      step_geodist(x, y, ref_lon = 0.5, ref_lat = 0.25, log = exp(1)) %>%
      prep(training = rand_data_2)
  )   
})

test_that('printing', {
  rec <- recipe( ~ x + y, data = rand_data) %>%
    step_geodist(x, y, ref_lat = 0.5, ref_lon = 0.25, log = FALSE)
  expect_output(print(rec))
  expect_output(prep(rec, training = rand_data, verbose = TRUE))
})

