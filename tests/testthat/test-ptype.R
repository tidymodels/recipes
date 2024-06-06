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
  
  rec_spec <- recipe(y ~ ., data = data_orig) %>%
    step_dummy(all_nominal_predictors()) %>%
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
  
  rec_spec <- recipe(y ~ ., data = data_orig) %>%
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
  
  rec_spec <- recipe(y ~ ., data = data_orig) %>%
    update_role(id, new_role = "id") %>%
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

test_that("recipes_ptype errors on old recipes", {
  rec <- recipe(mpg ~ ., data = mtcars)
  
  # simulate pre-1.1.0 recipe
  rec$ptype <- NULL

  expect_snapshot(
    error = TRUE,
    recipes_ptype(rec)
  )
})
