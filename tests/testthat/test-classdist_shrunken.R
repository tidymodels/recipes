test_that("shrunken centroids", {
  set.seed(1)
  nsc_test <-
    dplyr::tibble(
      x = rnorm(300),
      y = rnorm(300),
      class = rep(letters[1:3], each = 100)
    )
  # make completely separable
  nsc_test$x[nsc_test$class == "a"] <- nsc_test$x[nsc_test$class == "a"] + 8
  nsc_test$y[nsc_test$class == "b"] <- nsc_test$y[nsc_test$class == "b"] - 8

  # ----------------------------------------------------------------------------

  nsc_rec_zero <-
    recipe(class ~ x + y, data = nsc_test) |>
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = class,
      threshold = 0
    ) |>
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
  expect_equal(cent_zero[0, ], exp_res)
  expect_equal(nrow(cent_zero), 6)
  expect_true(!any(cent_zero$shrunken == 0))

  expect_equal(
    names(bake(nsc_rec_zero, new_data = NULL)),
    c("x", "y", "class", "classdist_a", "classdist_b", "classdist_c")
  )

  # ----------------------------------------------------------------------------

  nsc_rec_one <-
    recipe(class ~ x + y, data = nsc_test) |>
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = class,
      threshold = 1,
      log = FALSE,
      prefix = "potato_"
    ) |>
    prep()

  cent_one <- nsc_rec_one$steps[[1]]$objects
  expect_equal(cent_one[0, ], exp_res)
  expect_equal(nrow(cent_one), 6)
  expect_true(all(cent_one$shrunken == 0))

  expect_equal(
    names(bake(nsc_rec_one, new_data = NULL)),
    c("x", "y", "class", "potato_a", "potato_b", "potato_c")
  )

  # ----------------------------------------------------------------------------

  nsc_rec_half <-
    recipe(class ~ x + y, data = nsc_test) |>
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = class,
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
    recipe(class ~ x + y, data = nsc_test) |>
      step_classdist_shrunken(
        all_numeric_predictors(),
        class = class,
        threshold = -1
      ) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(class ~ x + y, data = nsc_test) |>
      step_classdist_shrunken(
        all_numeric_predictors(),
        class = class,
        sd_offset = -1
      ) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(class ~ x + y, data = nsc_test) |>
      step_classdist_shrunken(
        all_numeric_predictors(),
        class = class,
        log = 2
      ) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(class ~ x + y, data = nsc_test) |>
      step_classdist_shrunken(
        all_numeric_predictors(),
        class = class,
        prefix = 2
      ) |>
      prep(),
    error = TRUE
  )

  # ------------------------------------------------------------------------------

  nsc_test$weights <- importance_weights(1:nrow(nsc_test))
  nsc_rec_weights <-
    recipe(class ~ ., data = nsc_test) |>
    step_classdist_shrunken(
      all_numeric_predictors(),
      class = class,
      threshold = 1 / 2,
      keep_original_cols = FALSE
    )
  nsc_rec_weights_prep <- prep(nsc_rec_weights)

  tidy_weights_prep <- tidy(nsc_rec_weights_prep, 1)
  global_unwt <- tidy_prep |>
    dplyr::filter(type == "global") |>
    purrr::pluck("value")
  global_wt <- tidy_weights_prep |>
    dplyr::filter(type == "global") |>
    purrr::pluck("value")

  expect_true(all(global_unwt != global_wt))
  expect_equal(unique(tidy_weights_prep$terms), c("x", "y"))

  # ------------------------------------------------------------------------------

  expect_equal(
    required_pkgs(nsc_rec_weights),
    c("recipes", "dplyr", "tidyr")
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_classdist_shrunken(all_predictors())
  rec_param <- tunable.step_classdist_shrunken(rec$steps[[1]])
  expect_equal(rec_param$name, "threshold")
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("recipes_argument_select() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_classdist_shrunken(disp, class = NULL) |>
      prep()
  )
})

test_that("addition of recipes_argument_select() is backwards compatible", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist_shrunken(all_predictors(), class = Species) |>
    prep()

  exp <- bake(rec, iris)

  rec$steps[[1]]$class <- "Species"

  expect_identical(
    bake(rec, iris),
    exp
  )

  rec_old <- recipe(Species ~ ., data = iris) |>
    step_classdist_shrunken(all_predictors(), class = "Species") |>
    prep()

  expect_identical(
    bake(rec_old, iris),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist_shrunken(Petal.Length, class = Species, log = FALSE) |>
    update_role(Petal.Length, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = iris, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(trained, new_data = iris[, c(-3)]))
})

test_that("empty printing", {
  rec <- recipe(Species ~ ., iris)
  rec <- step_classdist_shrunken(rec, class = Species)

  expect_snapshot(rec)

  rec <- prep(rec, iris)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(Species ~ ., iris)
  rec2 <- step_classdist_shrunken(rec1, class = Species)

  rec1 <- prep(rec1, iris)
  rec2 <- prep(rec2, iris)

  baked1 <- bake(rec1, iris)
  baked2 <- bake(rec2, iris)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(Species ~ ., iris)
  rec <- step_classdist_shrunken(rec, class = Species)

  expect <- tibble(
    terms = character(),
    value = double(),
    class = character(),
    type = character(),
    threshold = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, iris)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c(
    "Species",
    "classdist_setosa",
    "classdist_versicolor",
    "classdist_virginica"
  )

  rec <- recipe(Species ~ Sepal.Length, data = iris) |>
    step_classdist_shrunken(
      all_predictors(),
      class = Species,
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(Species ~ Sepal.Length, data = iris) |>
    step_classdist_shrunken(
      all_predictors(),
      class = Species,
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("Sepal.Length", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  # step_classdist_shrunken() was added after keep_original_cols
  # Making this test case unlikely
  expect_true(TRUE)
})

test_that("printing", {
  rec <- recipe(Species ~ ., data = iris) |>
    step_classdist_shrunken(all_predictors(), class = Species)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_classdist_shrunken(
      all_predictors(),
      threshold = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_classdist_shrunken(all_numeric_predictors(), class = Species) |>
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
