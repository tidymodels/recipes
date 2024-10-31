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

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data) %>%
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

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data) %>%
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
    recipe(~ ., data = hotel_data)
  )

  expect_snapshot(
    error = TRUE,
    recipe(hotel_data)
  )
})
