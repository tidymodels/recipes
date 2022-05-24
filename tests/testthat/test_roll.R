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

rec <- recipe(~., data = sim_dat)

test_that("error checks", {
  expect_snapshot(error = TRUE,
    rec %>% step_window(y1, size = 6)
  )
  expect_snapshot(error = TRUE,
    rec %>% step_window(y1, size = NA)
  )
  # Wait for call pass through
  expect_error(
    rec %>% step_window(y1, size = NULL)
  )
  expect_snapshot(error = TRUE,
    rec %>% step_window(y1, statistic = "average")
  )
  expect_snapshot(error = TRUE,
    rec %>% step_window(y1, size = 1)
  )
  expect_snapshot(error = TRUE,
    rec %>% step_window(y1, size = 2)
  )
  expect_snapshot(error = TRUE,
    rec %>% step_window(y1, size = -1)
  )
  expect_snapshot(
    rec %>% step_window(y1, size = pi)
  )
  expect_snapshot(error = TRUE,
    prep(rec %>% step_window(fac), training = sim_dat)
  )
  expect_snapshot(error = TRUE,
    prep(rec %>% step_window(y1, size = 1000L), training = sim_dat)
  )
  bad_names <- rec %>%
    step_window(starts_with("y"), names = "only_one_name")
  expect_snapshot(error = TRUE,
    prep(bad_names, training = sim_dat)
  )
})

test_that("basic moving average", {
  skip_if_not_installed("RcppRoll")
  simple_ma <- rec %>%
    step_window(starts_with("y"))
  simple_ma <- prep(simple_ma, training = sim_dat)
  simple_ma_res <- bake(simple_ma, new_data = sim_dat)
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

test_that("creating new variables", {
  skip_if_not_installed("RcppRoll")
  new_names <- rec %>%
    step_window(starts_with("y"), names = paste0("new", 1:2), role = "predictor")
  new_names <- prep(new_names, training = sim_dat)
  new_names_res <- bake(new_names, new_data = sim_dat)

  simple_ma <- rec %>%
    step_window(starts_with("y"))
  simple_ma <- prep(simple_ma, training = sim_dat)
  simple_ma_res <- bake(simple_ma, new_data = sim_dat)

  expect_equal(new_names_res$new1, simple_ma_res$y1)
  expect_equal(new_names_res$new2, simple_ma_res$y2)
})

test_that("na_rm argument works for step_window", {
  skip_if_not_installed("RcppRoll")

  sim_dat_na <- sim_dat
  sim_dat_na[7, 2:3] <- NA

  simple_ma_no_rm_na <- recipe(~., data = sim_dat_na) %>%
    step_window(starts_with("y"), na_rm = FALSE) %>%
    prep() %>%
    bake(new_data = NULL)

  simple_ma_rm_na <- recipe(~., data = sim_dat_na) %>%
    step_window(starts_with("y"), na_rm = TRUE) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_false(any(is.na(simple_ma_rm_na$y1)))
  expect_false(any(is.na(simple_ma_rm_na$y2)))

  exp_rm_na <- simple_ma_rm_na
  exp_rm_na[6:8, 2:3] <- NA

  expect_equal(
    simple_ma_no_rm_na,
    exp_rm_na
  )
})

test_that("printing", {
  skip_if_not_installed("RcppRoll")
  new_names <- rec %>%
    step_window(starts_with("y"), names = paste0("new", 1:2), role = "predictor")
  expect_snapshot(print(new_names))
  expect_snapshot(prep(new_names))
})


test_that("tunable", {
  rec <-
    recipe(~., data = iris) %>%
    step_window(all_predictors(), outcome = "Species")
  rec_param <- tunable.step_window(rec$steps[[1]])
  expect_equal(rec_param$name, c("statistic", "window"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_window(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_window(rec)

  expect <- tibble(
    terms = character(),
    statistic = character(),
    size = integer(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_window(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
