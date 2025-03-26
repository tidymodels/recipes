# ------------------------------------------------------------------------------
# update_role_requirements()

test_that("`role` is validated", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)

  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, 1)
  })
  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, c("x", "y"))
  })
})

# ------------------------------------------------------------------------------
# update_role_requirements(bake =)

test_that("`bake = NULL` won't alter anything", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)
  rec <- update_role(rec, x, new_role = "id")
  rec <- update_role_requirements(rec, "id", bake = NULL)

  expect_identical(
    get_bake_role_requirements(rec),
    new_bake_role_requirements()
  )
})

test_that("can't update a role that doesn't exist", {
  df <- tibble(y = 1, x = 2)
  rec <- recipe(y ~ ., df)

  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, "id", bake = FALSE)
  })
})

test_that("can't update the predictor role", {
  df <- tibble(y = 1, x = 2)
  rec <- recipe(y ~ ., df)

  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, "predictor", bake = FALSE)
  })
})

test_that("can't update the outcome role", {
  df <- tibble(y = 1, x = 2)
  rec <- recipe(y ~ ., df)

  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, "outcome", bake = FALSE)
  })
})

test_that("initial requirements are empty", {
  df <- tibble(y = 1, x = 2)
  rec <- recipe(y ~ ., df)

  expect_identical(
    rec$requirements$bake,
    new_bake_role_requirements()
  )
})

test_that("will still error if a step actually used a role that set `bake = FALSE`", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)
  rec <- update_role(rec, x, new_role = "id")
  rec <- update_role_requirements(rec, "id", bake = FALSE)
  rec <- step_scale(rec, x)
  expect_snapshot(
    rec <- prep(rec, df)
  )
  df$x <- NULL

  # Error is specific to details of `step_scale()`
  expect_snapshot(
    error = TRUE,
    bake(rec, df)
  )
})

test_that("can `bake()` without roles that set `bake = FALSE`", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)
  rec <- update_role(rec, x, new_role = "id")
  rec <- update_role_requirements(rec, "id", bake = FALSE)
  rec <- prep(rec, df)

  bake <- get_bake_role_requirements(rec)
  expect_false(bake[["id"]])

  df$x <- NULL

  expect <- bake(rec, new_data = NULL)
  expect$x <- NULL

  expect_identical(
    bake(rec, new_data = df),
    expect
  )
})

test_that("can update `bake` requirements after prepping", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)
  rec <- update_role(rec, x, new_role = "id")
  rec <- prep(rec, df)

  df$x <- NULL

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })

  expect <- bake(rec, new_data = NULL)
  expect$x <- NULL

  rec <- update_role_requirements(rec, "id", bake = FALSE)

  expect_identical(
    bake(rec, df),
    expect
  )
})

test_that("errors on missing 'predictor's", {
  df <- tibble(y = 1, x = 2, z = 3, w = 4)

  rec <- recipe(y ~ ., df)
  rec <- prep(rec, df)

  df <- df["y"]

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })
})

test_that("doesn't error on missing case weights by default", {
  df <- tibble(y = 1, w = hardhat::frequency_weights(1))

  rec <- recipe(y ~ ., df)
  rec <- prep(rec, df)

  df$w <- NULL

  expect <- bake(rec, new_data = NULL)
  expect$w <- NULL

  expect_identical(
    bake(rec, df),
    expect
  )
})

test_that("can request that case weights be required", {
  df <- tibble(y = 1, w = hardhat::frequency_weights(1))

  rec <- recipe(y ~ ., df)
  rec <- update_role_requirements(rec, "case_weights", bake = TRUE)
  rec <- prep(rec, df)

  df$w <- NULL

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })
})

test_that("nonstandard roles are required by default", {
  df <- tibble(y = 1, x = 2, z = 3)

  rec <- recipe(df)
  rec <- update_role(rec, x, z, new_role = "id")
  rec <- prep(rec, df)

  df <- df["y"]

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })
})

test_that("unspecified roles are required by default", {
  df <- tibble(y = 1, x = 2, z = 3)

  rec <- recipe(df)
  rec <- prep(rec, df)

  df <- df["y"]

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })
})

test_that("both `NA_character_` and `'NA'` refer to unspecified roles", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(df)
  rec <- update_role_requirements(rec, NA_character_, bake = FALSE)
  rec <- prep(rec, df)
  expect_identical(bake(rec, df), df)

  rec <- recipe(df)
  rec <- update_role_requirements(rec, "NA", bake = FALSE)
  rec <- prep(rec, df)
  expect_identical(bake(rec, df), df)
})

test_that("can update the same role twice", {
  df <- tibble(y = 1, w = hardhat::frequency_weights(1))

  rec <- recipe(df)

  rec <- update_role_requirements(rec, "case_weights", bake = TRUE)
  expect_true(get_bake_role_requirements(rec)[["case_weights"]])

  rec <- update_role_requirements(rec, "case_weights", bake = FALSE)
  expect_false(get_bake_role_requirements(rec)[["case_weights"]])
})

test_that("can bake on an old recipe that doesn't have `requirements`", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)

  # Mock an old recipe that might not have this
  rec$requirements <- NULL

  rec <- prep(rec, df)

  # Works fine if all columns are there
  expect_identical(
    bake(rec, df),
    df[c("x", "y")]
  )

  df$x <- NULL

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })
})

test_that("can bake on an old recipe that doesn't have `requirements$bake`", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)

  # Mock an old recipe that might not have this
  rec$requirements$bake <- NULL

  rec <- prep(rec, df)

  # Works fine if all columns are there
  expect_identical(
    bake(rec, df),
    df[c("x", "y")]
  )

  df$x <- NULL

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })
})

test_that("can update the role requirements of an old recipe", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)
  rec <- update_role(rec, x, new_role = "id")

  rec <- prep(rec, df)

  # Mock an old recipe that won't have this.
  # This is the case of a user that has saved an old recipe and can't
  # regenerate it, but needs to make predictions with it.
  rec$requirements <- NULL

  df$x <- NULL

  expect_snapshot(error = TRUE, {
    bake(rec, df)
  })

  rec <- update_role_requirements(rec, "id", bake = FALSE)

  expect_identical(bake(rec, df), df["y"])
})

test_that("`bake` is validated", {
  df <- tibble(y = 1, x = 2)

  rec <- recipe(y ~ ., df)
  rec <- update_role(rec, x, new_role = "id")

  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, "id", bake = 1)
  })
  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, "id", bake = c(TRUE, FALSE))
  })
  expect_snapshot(error = TRUE, {
    update_role_requirements(rec, "id", bake = NA)
  })
})
