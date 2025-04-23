test_that("recipe() accepts sparse tibbles", {
  skip_if_not_installed("modeldata")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  expect_no_condition(
    rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data, avg_price_per_room ~ .)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec_spec$template)
  )
})

test_that("prep() accepts sparse tibbles", {
  skip_if_not_installed("modeldata")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)

  expect_no_error(
    rec <- prep(rec_spec)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec$template)
  )

  expect_no_error(
    rec <- prep(rec_spec, training = hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec$template)
  )
})

test_that("bake() accepts sparse tibbles", {
  skip_if_not_installed("modeldata")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data) |>
    prep()

  expect_no_condition(
    res <- bake(rec_spec, new_data = NULL)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(res)
  )

  expect_no_error(
    res <- bake(rec_spec, new_data = hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(res)
  )
})

test_that("recipe() accepts sparse matrices", {
  skip_if_not_installed("modeldata")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  expect_no_condition(
    rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data, avg_price_per_room ~ .)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec_spec$template)
  )
})

test_that("prep() accepts sparse matrices", {
  skip_if_not_installed("modeldata")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)

  expect_no_error(
    rec <- prep(rec_spec)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec$template)
  )

  expect_no_error(
    rec <- prep(rec_spec, training = hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(rec$template)
  )
})

test_that("bake() accepts sparse matrices", {
  skip_if_not_installed("modeldata")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data) |>
    prep()

  expect_no_condition(
    res <- bake(rec_spec, new_data = NULL)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(res)
  )

  expect_no_error(
    res <- bake(rec_spec, new_data = hotel_data)
  )

  expect_true(
    sparsevctrs::has_sparse_elements(res)
  )
})

test_that("recipe() errors if sparse matrix has no colnames", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()
  colnames(hotel_data) <- NULL

  expect_snapshot(
    error = TRUE,
    recipe(~., data = hotel_data)
  )

  expect_snapshot(
    error = TRUE,
    recipe(hotel_data)
  )
})

test_that(".recipes_toggle_sparse_args works", {
  skip_if_not_installed("modeldata")
  data("ames", package = "modeldata")

  rec_spec <- recipe(Sale_Price ~ ., data = ames) |>
    step_center(all_numeric_predictors()) |>
    step_center(all_numeric_predictors())

  expect_identical(
    .recipes_toggle_sparse_args(rec_spec, "yes"),
    rec_spec
  )

  # deals with wirdness between magrittr and base pipe
  clean_environments <- function(x) {
    for (i in seq_along(x$steps)) {
      attr(x$steps[[i]]$terms[[1]], '.Environment') <- NULL
    }
    x
  }

  rec_spec_yes_yes <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(MS_Zoning, Street, sparse = "yes", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    step_dummy(all_nominal_predictors(), sparse = "yes", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    clean_environments()

  rec_spec_no_no <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(MS_Zoning, Street, sparse = "no", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    step_dummy(all_nominal_predictors(), sparse = "no", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    clean_environments()

  rec_spec_yes_no <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(MS_Zoning, Street, sparse = "yes", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    step_dummy(all_nominal_predictors(), sparse = "no", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    clean_environments()

  rec_spec_no_yes <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(MS_Zoning, Street, sparse = "no", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    step_dummy(all_nominal_predictors(), sparse = "yes", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    clean_environments()

  rec_spec_auto_yes <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(MS_Zoning, Street, sparse = "auto", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    step_dummy(all_nominal_predictors(), sparse = "yes", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    clean_environments()

  rec_spec_auto_no <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(MS_Zoning, Street, sparse = "auto", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    step_dummy(all_nominal_predictors(), sparse = "no", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    clean_environments()

  rec_spec_auto_auto <- recipe(Sale_Price ~ ., data = ames) |>
    step_dummy(MS_Zoning, Street, sparse = "auto", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    step_dummy(all_nominal_predictors(), sparse = "auto", id = "") |>
    step_center(all_numeric_predictors(), id = "") |>
    clean_environments()

  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_yes_yes, "yes"),
    rec_spec_yes_yes
  )
  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_yes_yes, "no"),
    rec_spec_yes_yes
  )

  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_no_no, "yes"),
    rec_spec_no_no
  )
  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_no_no, "no"),
    rec_spec_no_no
  )

  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_auto_auto, "yes"),
    rec_spec_yes_yes
  )
  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_auto_auto, "no"),
    rec_spec_no_no
  )

  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_auto_yes, "yes"),
    rec_spec_yes_yes
  )
  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_auto_yes, "no"),
    rec_spec_no_yes
  )

  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_auto_no, "yes"),
    rec_spec_yes_no
  )
  expect_identical(
    .recipes_toggle_sparse_args(rec_spec_auto_no, "no"),
    rec_spec_no_no
  )
})

test_that(".recipes_toggle_sparse_args works", {
  rec <- recipe(~., mtcars)

  expect_identical(
    .recipes_estimate_sparsity(rec),
    sparsevctrs::sparsity(mtcars)
  )

  rec <- recipe(~., iris) |>
    step_normalize(all_numeric_predictors()) |>
    step_dummy(all_nominal_predictors(), sparse = "auto")

  exp <- rec |> prep() |> bake(NULL) |> sparsevctrs::sparsity()

  expect_equal(
    .recipes_estimate_sparsity(rec),
    exp
  )

  rec <- recipe(~., iris[0, ]) |>
    step_normalize(all_numeric_predictors()) |>
    step_dummy(all_nominal_predictors(), sparse = "auto")

  expect_equal(
    .recipes_estimate_sparsity(rec),
    exp
  )
})

test_that(".recipes_toggle_sparse_args works", {
  rec <- recipe(~., mtcars) |>
    step_dummy()

  expect_false(
    .recipes_preserve_sparsity(rec$steps[[1]])
  )
})

test_that(".recipes_estimate_sparsity doesn't error on created vars (#1448)", {
  rec <- recipe(~., iris) |>
    step_mutate(Species2 = Species) |>
    step_dummy(Species2)

  expect_no_error(
    .recipes_estimate_sparsity(rec)
  )
})
