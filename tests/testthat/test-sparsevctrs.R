test_that("recipe() accepts sparse tibbles", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  expect_no_condition(
    rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)
  )

  expect_true(
    is_sparse_tibble(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data)
  )

  expect_true(
    is_sparse_tibble(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data, avg_price_per_room ~ .)
  )

  expect_true(
    is_sparse_tibble(rec_spec$template)
  )
})

test_that("prep() accepts sparse tibbles", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)
  
  expect_no_error(
    rec <- prep(rec_spec)
  )

  expect_true(
    is_sparse_tibble(rec$template)
  )
  
  expect_no_error(
    rec <- prep(rec_spec, training = hotel_data)
  )

  expect_true(
    is_sparse_tibble(rec$template)
  )
})

test_that("bake() accepts sparse tibbles", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data) %>%
    prep()
  
  expect_no_condition(
    res <- bake(rec_spec, new_data = NULL)
  )

  expect_true(
    is_sparse_tibble(res)
  )
  
  expect_no_error(
    res <- bake(rec_spec, new_data = hotel_data)
  )

  expect_true(
    is_sparse_tibble(res)
  )
})

test_that("recipe() accepts sparse matrices", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()

  expect_no_condition(
    rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)
  )

  expect_true(
    is_sparse_tibble(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data)
  )

  expect_true(
    is_sparse_tibble(rec_spec$template)
  )

  expect_no_condition(
    rec_spec <- recipe(hotel_data, avg_price_per_room ~ .)
  )

  expect_true(
    is_sparse_tibble(rec_spec$template)
  )
})

test_that("prep() accepts sparse matrices", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data)
  
  expect_no_error(
    rec <- prep(rec_spec)
  )

  expect_true(
    is_sparse_tibble(rec$template)
  )
  
  expect_no_error(
    rec <- prep(rec_spec, training = hotel_data)
  )

  expect_true(
    is_sparse_tibble(rec$template)
  )
})

test_that("bake() accepts sparse matrices", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()

  rec_spec <- recipe(avg_price_per_room ~ ., data = hotel_data) %>%
    prep()
  
  expect_no_condition(
    res <- bake(rec_spec, new_data = NULL)
  )

  expect_true(
    is_sparse_tibble(res)
  )
  
  expect_no_error(
    res <- bake(rec_spec, new_data = hotel_data)
  )

  expect_true(
    is_sparse_tibble(res)
  )
})
