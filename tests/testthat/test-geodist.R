library(testthat)
library(recipes)

set.seed(4693)
rand_data <- data.frame(x = round(runif(10), 2), y = round(runif(10), 2))
rand_data$x[1] <- NA

dists <-
  apply(
    as.matrix(rand_data),
    1,
    function(x, y, z) {
      sqrt((x[1] - y)^2 + (x[2] - z)^2)
    },
    y = 0.5,
    z = 0.25
  )

test_that("basic functionality", {
  rec <- recipe(~ x + y, data = rand_data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE,
      log = FALSE
    )
  rec_trained <- prep(rec, training = rand_data)

  tr_int <- bake(rec_trained, new_data = NULL, all_predictors())
  te_int <- bake(rec_trained, rand_data, all_predictors())

  expect_equal(tr_int[["geo_dist"]], dists)
  expect_equal(te_int[["geo_dist"]], dists)

  rec_log <- recipe(~ x + y, data = rand_data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE,
      log = TRUE
    )
  rec_log_trained <- prep(rec_log, training = rand_data)

  tr_log_int <- bake(rec_log_trained, new_data = NULL, all_predictors())
  te_log_int <- bake(rec_log_trained, rand_data, all_predictors())

  expect_equal(tr_log_int[["geo_dist"]], log(dists))
  expect_equal(te_log_int[["geo_dist"]], log(dists))
})

test_that("lat lon", {
  postal <- tibble(latitude = 0, longitude = 0)
  near_station <- recipe(~., data = postal) |>
    step_geodist(
      lat = latitude,
      lon = longitude,
      log = FALSE,
      ref_lat = 0,
      ref_lon = 0,
      is_lat_lon = TRUE
    ) |>
    prep() |>
    bake(new_data = NULL)
  expect_equal(near_station[["geo_dist"]], 0)

  postal <- tibble(latitude = 38.8981014, longitude = -77.0104265)
  near_station <- recipe(~., data = postal) |>
    step_geodist(
      lat = latitude,
      lon = longitude,
      log = FALSE,
      ref_lat = 38.8986312,
      ref_lon = -77.0062457,
      is_lat_lon = TRUE
    ) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(near_station[["geo_dist"]], 367, tolerance = 1)

  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = 100,
        ref_lon = 100,
        is_lat_lon = TRUE
      ) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = 0,
        ref_lon = 190,
        is_lat_lon = TRUE
      ) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = -100,
        ref_lon = 0,
        is_lat_lon = TRUE
      ) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = 0,
        ref_lon = -190,
        is_lat_lon = TRUE
      ) |>
      prep()
  )

  postal <- tibble(latitude = 100, longitude = 0)
  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = 38.8986312,
        ref_lon = -77.0062457,
        is_lat_lon = TRUE
      ) |>
      prep()
  )

  postal <- tibble(latitude = 0, longitude = 190)
  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = 38.8986312,
        ref_lon = -77.0062457,
        is_lat_lon = TRUE
      ) |>
      prep()
  )
  postal <- tibble(latitude = -100, longitude = 0)
  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = 38.8986312,
        ref_lon = -77.0062457,
        is_lat_lon = TRUE
      ) |>
      prep()
  )

  postal <- tibble(latitude = 0, longitude = -190)
  expect_snapshot(
    error = TRUE,
    near_station <- recipe(~., data = postal) |>
      step_geodist(
        lat = latitude,
        lon = longitude,
        log = FALSE,
        ref_lat = 38.8986312,
        ref_lon = -77.0062457,
        is_lat_lon = TRUE
      ) |>
      prep()
  )
})

test_that("check_name() is used", {
  dat <- mtcars
  dat$geo_dist <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_geodist(vs, am, ref_lat = 0, ref_lon = 0)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("bad args", {
  rand_data_2 <- rand_data
  rand_data_2$x1 <- runif(nrow(rand_data_2))
  rand_data_2$y1 <- runif(nrow(rand_data_2))
  rec <- recipe(~., data = rand_data_2)

  expect_snapshot(
    error = TRUE,
    rec |>
      step_geodist(starts_with("x"), y, ref_lat = 0.5, ref_lon = 0.25) |>
      prep(training = rand_data_2)
  )
  expect_snapshot(
    error = TRUE,
    rec |>
      step_geodist(x, starts_with("y"), ref_lat = 0.5, ref_lon = 0.25) |>
      prep(training = rand_data_2)
  )
  expect_snapshot(
    error = TRUE,
    rec |>
      step_geodist(x, y, ref_lat = letters[1:2], ref_lon = 0.25) |>
      prep(training = rand_data_2)
  )
  expect_snapshot(
    error = TRUE,
    rec |>
      step_geodist(x, y, ref_lon = letters[1:2], ref_lat = 0.25) |>
      prep(training = rand_data_2)
  )
  expect_snapshot(
    error = TRUE,
    rec |>
      step_geodist(x, y, ref_lon = 0.5, ref_lat = 0.25, name = 1) |>
      prep(training = rand_data_2)
  )
  expect_snapshot(
    error = TRUE,
    rec |>
      step_geodist(x, y, ref_lon = 0.5, ref_lat = 0.25, log = exp(1)) |>
      prep(training = rand_data_2)
  )
  expect_snapshot(
    error = TRUE,
    recipe(~ x + y, data = rand_data) |>
      step_geodist(
        x,
        y,
        ref_lat = 0.5,
        ref_lon = 0.25,
        is_lat_lon = "no",
        log = FALSE
      ) |>
      prep(training = rand_data)
  )
  expect_snapshot(
    error = TRUE,
    recipe(~ x + y, data = rand_data) |>
      step_geodist(
        x,
        y,
        ref_lat = 0.5,
        ref_lon = 0.25,
        is_lat_lon = c(TRUE, TRUE),
        log = FALSE
      ) |>
      prep(training = rand_data)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~ x + y, data = rand_data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE,
      log = FALSE
    ) |>
    update_role(x, y, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  rec_trained <- prep(rec, rand_data)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = rand_data[, 2, drop = FALSE])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_geodist(rec, ref_lat = 0.5, ref_lon = 0.25, is_lat_lon = FALSE)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_geodist(rec1, ref_lat = 0.5, ref_lon = 0.25, is_lat_lon = FALSE)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_geodist(rec, ref_lat = 0.5, ref_lon = 0.25, is_lat_lon = FALSE)

  expect <- tibble(
    latitude = character(),
    longitude = character(),
    ref_latitude = double(),
    ref_longitude = double(),
    is_lat_lon = logical(),
    name = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("geo_dist")

  rec <- recipe(~ x + y, data = rand_data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE,
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~ x + y, data = rand_data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE,
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("x", "y", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~ x + y, data = rand_data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE
    )

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = rand_data)
  )
})

test_that("printing", {
  rec <- recipe(~ x + y, data = rand_data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE
    )

  rec
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- rand_data
  rec <- recipe(~., data) |>
    step_geodist(
      x,
      y,
      ref_lat = 0.5,
      ref_lon = 0.25,
      is_lat_lon = FALSE,
      log = FALSE
    ) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
