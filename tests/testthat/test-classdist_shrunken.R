test_that("shrunken centroids", {
  library(dplyr)
  library(purrr)
  library(hardhat)

  # ----------------------------------------------------------------------------

  set.seed(1)
  nsc_test <-
    tibble(
      x = rnorm(300),
      y = rnorm(300),
      class = rep(letters[1:3], each = 100)
    )
  # make completely separable
  nsc_test$x[nsc_test$class == "a"] <- nsc_test$x[nsc_test$class == "a"] + 8
  nsc_test$y[nsc_test$class == "b"] <- nsc_test$y[nsc_test$class == "b"] - 8

  # ----------------------------------------------------------------------------

  nsc_rec_zero <-
    recipe(class ~ x + y, data = nsc_test) %>%
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = "class",
      threshold = 0
    ) %>%
    prep()

  exp_res <-
    dplyr::tibble(
      variable = character(0),
      class = character(0),
      global = numeric(0),
      by_class = numeric(0),
      shrunken = numeric(0),
      std_dev = numeric(0)
    )
  cent_zero <- nsc_rec_zero$steps[[1]]$objects
  expect_equal(cent_zero[0,], exp_res)
  expect_equal(nrow(cent_zero), 6)
  expect_true(!any(cent_zero$shrunken == 0))

  expect_equal(
    names(bake(nsc_rec_zero, new_data = NULL)),
    c("x", "y", "class", "classdist_a", "classdist_b", "classdist_c")
  )

  # ----------------------------------------------------------------------------

  nsc_rec_one <-
    recipe(class ~ x + y, data = nsc_test) %>%
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = "class",
      threshold = 1,
      log = FALSE,
      prefix = "potato_"
    ) %>%
    prep()

  cent_one <- nsc_rec_one$steps[[1]]$objects
  expect_equal(cent_one[0,], exp_res)
  expect_equal(nrow(cent_one), 6)
  expect_true(all(cent_one$shrunken == 0))

  expect_equal(
    names(bake(nsc_rec_one, new_data = NULL)),
    c("x", "y", "class", "potato_a", "potato_b", "potato_c")
  )

  # ----------------------------------------------------------------------------

  nsc_rec_half <-
    recipe(class ~ x + y, data = nsc_test) %>%
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = "class",
      threshold = 1 / 2,
      keep_original_cols = FALSE
    )
  nsc_rec_half_prep <- prep(nsc_rec_half)

  expect_snapshot(print(nsc_rec_half))
  expect_snapshot(print(nsc_rec_half_prep))

  tidy_spec <- tidy(nsc_rec_half, 1)
  tidy_prep <- tidy(nsc_rec_half_prep, 1)
  expect_snapshot(print(tidy_spec))
  expect_snapshot(print(tidy_prep))

  expect_equal(
    names(bake(nsc_rec_half_prep, new_data = NULL)),
    c("class", "classdist_a", "classdist_b", "classdist_c")
  )


  # ----------------------------------------------------------------------------

  expect_snapshot(
    recipe(class ~ x + y, data = nsc_test) %>%
      step_classdist_shrunken(
        all_numeric_predictors(),
        class = "class",
        threshold = -1
      ) %>% prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(class ~ x + y, data = nsc_test) %>%
      step_classdist_shrunken(
        all_numeric_predictors(),
        class = "class",
        sd_offset = -1
      ) %>% prep(),
    error = TRUE
  )

  # ------------------------------------------------------------------------------

  nsc_test$weights <- importance_weights(1:nrow(nsc_test))
  nsc_rec_weights <-
    recipe(class ~ ., data = nsc_test) %>%
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = "class",
      threshold = 1 / 2,
      keep_original_cols = FALSE
    )
  nsc_rec_weights_prep <- prep(nsc_rec_weights)

  tidy_weights_prep <- tidy(nsc_rec_weights_prep, 1)
  global_unwt <- tidy_prep %>% dplyr::filter(type == "global") %>% pluck("value")
  global_wt <- tidy_weights_prep %>% dplyr::filter(type == "global") %>% pluck("value")

  expect_true(all(global_unwt != global_wt))
  expect_equal(unique(tidy_weights_prep$terms), c("x", "y"))

  # ------------------------------------------------------------------------------

  expect_equal(
    required_pkgs(nsc_rec_weights),
    c("recipes", "dplyr", "tidyr")
  )
})
