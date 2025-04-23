test_that("recipes_ptype() works", {
  data_orig <- tibble(
    y = 1:10,
    id = 1:10,
    x1 = letters[1:10],
    x2 = factor(letters[1:10]),
    cw = hardhat::importance_weights(1:10)
  )

  rec_spec <- recipe(y ~ ., data = data_orig)

  exp_ptype <- vctrs::vec_ptype(data_orig)

  expect_identical(
    recipes_ptype(rec_spec, stage = "prep")[names(exp_ptype)],
    exp_ptype
  )
  expect_identical(
    recipes_ptype(rec_spec, stage = "bake"),
    exp_ptype[c("id", "x1", "x2")]
  )
})

test_that("recipes_ptype() isn't affected by prepping recipe", {
  data_orig <- tibble(
    y = 1:10,
    id = 1:10,
    x1 = letters[1:10],
    x2 = factor(letters[1:10]),
    cw = hardhat::importance_weights(1:10)
  )

  rec_spec <- recipe(y ~ ., data = data_orig) |>
    step_dummy(all_nominal_predictors()) |>
    prep()

  exp_ptype <- vctrs::vec_ptype(data_orig)

  expect_identical(
    recipes_ptype(rec_spec, stage = "prep")[names(exp_ptype)],
    exp_ptype
  )
  expect_identical(
    recipes_ptype(rec_spec, stage = "bake"),
    exp_ptype[c("id", "x1", "x2")]
  )
})

test_that("recipes_ptype() works with update_role()", {
  data_orig <- tibble(
    y = 1:10,
    id = 1:10,
    x1 = letters[1:10],
    x2 = factor(letters[1:10]),
    cw = hardhat::importance_weights(1:10)
  )

  rec_spec <- recipe(y ~ ., data = data_orig) |>
    update_role(id, new_role = "id")

  exp_ptype <- vctrs::vec_ptype(data_orig)

  expect_identical(
    recipes_ptype(rec_spec, stage = "prep")[names(exp_ptype)],
    exp_ptype
  )
  expect_identical(
    recipes_ptype(rec_spec, stage = "bake"),
    exp_ptype[c("id", "x1", "x2")]
  )
})

test_that("recipes_ptype() works with update_role_requirements()", {
  data_orig <- tibble(
    y = 1:10,
    id = 1:10,
    x1 = letters[1:10],
    x2 = factor(letters[1:10]),
    cw = hardhat::importance_weights(1:10)
  )

  rec_spec <- recipe(y ~ ., data = data_orig) |>
    update_role(id, new_role = "id") |>
    update_role_requirements("id", bake = FALSE)

  exp_ptype <- vctrs::vec_ptype(data_orig)

  expect_identical(
    recipes_ptype(rec_spec, stage = "prep")[names(exp_ptype)],
    exp_ptype
  )
  expect_identical(
    recipes_ptype(rec_spec, stage = "bake"),
    exp_ptype[c("x1", "x2")]
  )
})

test_that("recipes_ptype() works with NA roles", {
  data_orig <- tibble(
    y = 1:10,
    id = 1:10,
    x1 = letters[1:10],
    x2 = factor(letters[1:10]),
    cw = hardhat::importance_weights(1:10)
  )

  rec_spec <- recipe(data_orig)

  exp_ptype <- vctrs::vec_ptype(data_orig)

  expect_identical(
    recipes_ptype(rec_spec, stage = "prep")[names(exp_ptype)],
    exp_ptype
  )
  expect_identical(
    recipes_ptype(rec_spec, stage = "bake"),
    exp_ptype[c("y", "id", "x1", "x2")]
  )
})

test_that("recipes_ptype() works with formula interface", {
  data_orig <- tibble(
    y = 1:10,
    id = 1:10,
    x1 = letters[1:10],
    x2 = factor(letters[1:10]),
    cw = hardhat::importance_weights(1:10)
  )

  rec_spec <- recipe(data_orig, y ~ x1)

  exp_ptype <- vctrs::vec_ptype(data_orig)

  expect_identical(
    recipes_ptype(rec_spec, stage = "prep"),
    exp_ptype[c("x1", "y")]
  )
  expect_identical(
    recipes_ptype(rec_spec, stage = "bake"),
    exp_ptype["x1"]
  )
})

test_that("recipes_ptype returns NULL on old recipes", {
  rec <- recipe(mpg ~ ., data = mtcars)

  # simulate pre-1.1.0 recipe
  rec$ptype <- NULL

  expect_null(
    recipes_ptype(rec)
  )
})

test_that("recipes_ptype_validate() works", {
  data_orig <- tibble(
    y = 1:10,
    id = 1:10,
    x1 = letters[1:10],
    x2 = factor(letters[1:10]),
    cw = hardhat::importance_weights(1:10)
  )

  rec <- recipe(y ~ ., data = data_orig)

  expect_no_error(
    recipes_ptype_validate(rec, data_orig)
  )

  # different ordering
  data_new <- data_orig[, 5:1]
  expect_no_error(
    recipes_ptype_validate(rec, data_new)
  )

  # works at bake time
  data_new <- data_orig
  data_new$y <- NULL
  expect_no_error(
    recipes_ptype_validate(rec, data_new, stage = "bake")
  )

  # Extra variables
  data_new <- data_orig
  data_new$new <- 1:10
  expect_no_error(
    recipes_ptype_validate(rec, data_new)
  )

  # missing variables
  data_new <- data_orig
  data_new$id <- NULL
  expect_snapshot(
    error = TRUE,
    recipes_ptype_validate(rec, data_new)
  )

  data_new <- data_orig
  data_new$id <- NULL
  data_new$x1 <- NULL
  expect_snapshot(
    error = TRUE,
    recipes_ptype_validate(rec, data_new)
  )

  # wrong class
  data_new <- data_orig
  data_new$id <- as.double(data_new$id)
  expect_snapshot(
    error = TRUE,
    recipes_ptype_validate(rec, data_new)
  )

  data_new <- data_orig
  data_new$id <- as.double(data_new$id)
  data_new$y <- as.double(data_new$y)
  data_new$x2 <- as.integer(data_new$x2)
  expect_snapshot(
    error = TRUE,
    recipes_ptype_validate(rec, data_new)
  )

  # wrong attributes
  data_new <- data_orig
  attributes(data_new$x1) <- list("amount" = 4)

  expect_snapshot(
    error = TRUE,
    recipes_ptype_validate(rec, data_new)
  )

  # wrong attributes
  data_new <- data_orig
  attributes(data_new$x1) <- list("amount" = 4)
  attributes(data_new$id) <- list("amount" = 4)

  expect_snapshot(
    error = TRUE,
    recipes_ptype_validate(rec, data_new)
  )

  # wrong factor levels
  data_new <- data_orig
  levels(data_new$x2) <- letters

  expect_snapshot(
    error = TRUE,
    recipes_ptype_validate(rec, data_new)
  )
})
