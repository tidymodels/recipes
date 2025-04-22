library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(Sacramento, package = "modeldata")

set.seed(131)
Sacramento_rec <- recipe(~., data = Sacramento) |>
  step_other(all_nominal(), threshold = 0.05, other = "another") |>
  step_center(all_numeric()) |>
  step_dummy(all_nominal()) |>
  check_cols(starts_with("beds"))

test_that("untrained", {
  exp_res_1 <- tibble(
    number = 1:4,
    operation = c("step", "step", "step", "check"),
    type = c("other", "center", "dummy", "cols"),
    trained = rep(FALSE, 4),
    skip = rep(FALSE, 4),
    id = vapply(Sacramento_rec$steps, function(x) x$id, character(1))
  )
  expect_equal(tidy(Sacramento_rec), exp_res_1)
})

test_that("trained", {
  exp_res_2 <- tibble(
    number = 1:4,
    operation = c("step", "step", "step", "check"),
    type = c("other", "center", "dummy", "cols"),
    trained = rep(TRUE, 4),
    skip = rep(FALSE, 4),
    id = vapply(Sacramento_rec$steps, function(x) x$id, character(1))
  )
  expect_snapshot(
    trained <- prep(Sacramento_rec, training = Sacramento)
  )
  expect_equal(tidy(trained), exp_res_2)
})

test_that("select step", {
  exp_res_3 <- tibble(
    terms = "all_numeric()",
    value = NA_real_,
    id = Sacramento_rec$steps[[2]][["id"]]
  )
  expect_equal(tidy(Sacramento_rec, number = 2), exp_res_3)
  expect_equal(
    tidy(Sacramento_rec, id = Sacramento_rec$steps[[2]][["id"]]),
    exp_res_3
  )
})

test_that("empty recipe", {
  expect_equal(
    tidy(recipe(x = mtcars)),
    tibble(
      number = integer(),
      operation = character(),
      type = character(),
      trained = logical(),
      skip = logical(),
      id = character()
    )
  )
})

test_that("bad args", {
  trained <- prep(Sacramento_rec, training = Sacramento)

  expect_snapshot(error = TRUE, tidy(trained, number = NULL))
  expect_snapshot(error = TRUE, tidy(trained, number = 100))
  expect_snapshot(error = TRUE, tidy(trained, number = 1, id = "id"))
  expect_snapshot(error = TRUE, tidy(trained, id = "id"))
  expect_snapshot(error = TRUE, tidy(trained, id = c("id", "id2")))
})

test_that("bag args", {
  trained <- prep(Sacramento_rec, training = Sacramento)

  single_step <- trained$steps[[1]]
  attr(single_step, "class") <- c("step_notidy", "step")

  expect_snapshot(error = TRUE, tidy(single_step))

  single_check <- trained$steps[[4]]
  attr(single_check, "class") <- c("check_notidy", "check")

  expect_snapshot(error = TRUE, tidy(single_check))
})
