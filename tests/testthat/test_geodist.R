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
    step_geodist(x, y, ref_lat = 0.5, ref_lon = 0.25, is_lat_lon = FALSE,
                 log = FALSE)
  rec_trained <- prep(rec, traning = rand_data)

  tr_int <- juice(rec_trained, all_predictors())
  te_int <- bake(rec_trained, rand_data, all_predictors())

  expect_equal(tr_int[["geo_dist"]], dists)
  expect_equal(te_int[["geo_dist"]], dists)

  rec_log <- recipe( ~ x + y, data = rand_data) %>%
    step_geodist(x, y, ref_lat = 0.5, ref_lon = 0.25,  is_lat_lon = FALSE,
                 log = TRUE)
  rec_log_trained <- prep(rec_log, traning = rand_data)

  tr_log_int <- juice(rec_log_trained, all_predictors())
  te_log_int <- bake(rec_log_trained, rand_data, all_predictors())

  expect_equal(tr_log_int[["geo_dist"]], log(dists))
  expect_equal(te_log_int[["geo_dist"]], log(dists))

})

test_that('lat lon', {
  postal <- tibble(latitude = 0, longitude = 0)
  near_station <- recipe( ~ ., data = postal) %>%
    step_geodist(lat = latitude, lon = longitude, log = FALSE,
                 ref_lat = 0, ref_lon = 0,
                 is_lat_lon = TRUE) %>%
    prep() %>%
    bake(new_data = NULL)
  expect_equal(near_station[["geo_dist"]], 0)


  postal <- tibble(latitude = 38.8981014, longitude = -77.0104265)
  near_station <- recipe( ~ ., data = postal) %>%
    step_geodist(lat = latitude, lon = longitude, log = FALSE,
                 ref_lat = 38.8986312, ref_lon = -77.0062457,
                 is_lat_lon = TRUE) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(near_station[["geo_dist"]], 367, tolerance = 1)


  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = 100, ref_lon = 100,
                   is_lat_lon = TRUE) %>%
      prep(),
    "`ref_lat` should be between -90 and 90"
  )

  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = 0, ref_lon = 190,
                   is_lat_lon = TRUE) %>%
      prep(),
    "`ref_lon` should be between -180 and 180"
  )

  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = -100, ref_lon = 0,
                   is_lat_lon = TRUE) %>%
      prep(),
    "`ref_lat` should be between -90 and 90"
  )

  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = 0, ref_lon = -190,
                   is_lat_lon = TRUE) %>%
      prep(),
    "`ref_lon` should be between -180 and 180"
  )

  postal <- tibble(latitude = 100, longitude = 0)
  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = 38.8986312, ref_lon = -77.0062457,
                   is_lat_lon = TRUE) %>%
      prep(),
    "All `lat` values should be between -90 and 90"
  )

  postal <- tibble(latitude = 0, longitude = 190)
  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = 38.8986312, ref_lon = -77.0062457,
                   is_lat_lon = TRUE) %>%
      prep(),
    "All `lon` values should be between -180 and 180"
  )
  postal <- tibble(latitude = -100, longitude = 0)
  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = 38.8986312, ref_lon = -77.0062457,
                   is_lat_lon = TRUE) %>%
      prep(),
    "All `lat` values should be between -90 and 90"
  )

  postal <- tibble(latitude = 0, longitude = -190)
  expect_error(
    near_station <- recipe( ~ ., data = postal) %>%
      step_geodist(lat = latitude, lon = longitude, log = FALSE,
                   ref_lat = 38.8986312, ref_lon = -77.0062457,
                   is_lat_lon = TRUE) %>%
      prep(),
    "All `lon` values should be between -180 and 180"
  )

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
    step_geodist(x, y, ref_lat = 0.5, ref_lon = 0.25, is_lat_lon = FALSE,
                 log = FALSE)
  expect_output(print(rec))
  expect_output(prep(rec, training = rand_data, verbose = TRUE))
})

