library(testthat)
library(recipes)

languages <- tribble(
  ~lang_1,
  ~lang_2,
  ~lang_3,
  ~lang_4,
  "English",
  "Italian",
  NA,
  NA,
  "Spanish",
  NA,
  "French",
  NA,
  "Armenian",
  "English",
  "French",
  NA,
  NA,
  NA,
  NA,
  NA
)

result <- tribble(
  ~Armenian,
  ~English,
  ~French,
  ~Italian,
  ~Spanish,
  0L,
  1L,
  0L,
  1L,
  0L,
  0L,
  0L,
  1L,
  0L,
  1L,
  1L,
  1L,
  1L,
  0L,
  0L,
  0L,
  0L,
  0L,
  0L,
  0L
)

test_that("dummy variables with factor inputs", {
  dummy <- recipe(~., data = languages) |>
    step_dummy_multi_choice(all_predictors())

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = languages)

  expect_identical(
    unname(unclass(dummy_pred)),
    unname(unclass(result))
  )

  expect_identical(
    names(dummy_pred),
    dummy_names(names(languages)[1], names(result))
  )
})

test_that("dummy variables with non-factor inputs", {
  dummy <- recipe(~., data = mtcars) |>
    step_dummy_multi_choice(all_predictors())

  expect_snapshot(error = TRUE, prep(dummy))
})

test_that("check_name() is used", {
  dat <- iris
  dat$Species_setosa <- dat$Species

  rec <- recipe(~., data = dat) |>
    step_dummy_multi_choice(Species)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = languages) |>
    step_dummy_multi_choice(all_predictors())
  rec_param <- tunable.step_dummy_multi_choice(rec$steps[[1]])
  expect_equal(rec_param$name, c("threshold"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("no columns selected", {
  zdat <- tibble(
    y = c(1, 2, 3),
    x = c("a", "a", "a"),
    z = 3:1
  )

  rec <- recipe(y ~ ., data = zdat) |>
    step_zv(all_predictors()) |>
    step_dummy_multi_choice(all_nominal()) |>
    prep(training = zdat)

  expect_equal(
    unname(rec$steps[[2]]$input),
    character()
  )

  expect_equal(names(bake(rec, zdat)), c("z", "y"))

  exp_tidy <- tibble(
    terms = character(),
    columns = character(),
    id = character()
  )
  expect_equal(exp_tidy, tidy(rec, number = 2))
})

test_that("one columns selected", {
  zdat <- tibble(
    y = c(1, 2, 3),
    x = c("a", "a", "a"),
    z = 3:1
  )

  rec <- recipe(y ~ ., data = zdat) |>
    step_dummy_multi_choice(all_nominal()) |>
    prep(training = zdat)

  expect_equal(
    unname(rec$steps[[1]]$input),
    "x"
  )

  expect_equal(names(bake(rec, zdat)), c("z", "y", "x_a"))
})

test_that("factor levels are preserved", {
  # old data
  tr <- data.frame(
    x = factor(c("a", "b", "c"), levels = c("a", "b", "c", "d", "e", "f", "g"))
  )

  # new data
  te <- data.frame(
    x = factor(c("c", "d", "e"), levels = c("a", "b", "c", "d", "e", "f", "g"))
  )
  data1 <- tr |>
    recipe() |>
    step_dummy(x, one_hot = T) |>
    prep() |>
    bake(new_data = te)

  data2 <- tr |>
    recipe() |>
    step_dummy_multi_choice(x, threshold = 0) |>
    prep() |>
    bake(new_data = te)

  expect_identical(ncol(data1), ncol(data2))
})

test_that("sparse = 'yes' works", {
  rec <- recipe(~., data = languages)

  dense <- rec |>
    step_dummy_multi_choice(all_predictors(), sparse = "no") |>
    prep() |>
    bake(NULL)
  dense <- purrr::map(dense, as.integer) |> tibble::new_tibble()
  sparse <- rec |>
    step_dummy_multi_choice(all_predictors(), sparse = "yes") |>
    prep() |>
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_vector, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  rec <- recipe(~., data = languages) |>
    step_dummy_multi_choice(all_predictors()) |>
    prep()

  exp <- bake(rec, languages)

  # Simulate old recipe
  rec$steps[[1]]$sparse <- NULL

  expect_identical(
    bake(rec, languages),
    exp
  )
})

test_that(".recipes_toggle_sparse_args works", {
  rec <- recipe(~., data = languages) |>
    step_dummy_multi_choice(all_predictors(), sparse = "auto")

  exp <- rec |> prep() |> bake(NULL) |> sparsevctrs::sparsity()

  expect_true(
    .recipes_estimate_sparsity(rec) < exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # lang_1 is not converted automatically because it has a non-standard role
  # but it is used like a factor variable. See also `?step_string2factor`
  languages <- languages |> mutate(lang_1 = factor(lang_1))
  rec <- recipe(~., data = languages) |>
    step_dummy_multi_choice(lang_1, lang_2, lang_3) |>
    update_role(lang_1, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = languages)

  expect_snapshot(error = TRUE, bake(rec_trained, new_data = languages[, -1]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_multi_choice(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_dummy_multi_choice(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_multi_choice(rec)

  expect <- tibble(terms = character(), columns = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- paste0("lang_1_", c("Armenian", "English", "Spanish"))

  rec <- recipe(~lang_1, data = languages) |>
    step_dummy_multi_choice(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~lang_1, data = languages) |>
    step_dummy_multi_choice(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("lang_1", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~lang_1, data = languages) |>
    step_dummy_multi_choice(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = languages)
  )
})

test_that("printing", {
  rec <- recipe(~., data = languages) |>
    step_dummy_multi_choice(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_dummy_multi_choice(
      all_predictors(),
      threshold = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("bad args", {
  expect_snapshot(
    dummy_multi_choice_rec <- recipe(~., data = languages) |>
      step_dummy_multi_choice(starts_with("lang"), other = 2) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    dummy_multi_choice_rec <- recipe(~., data = languages) |>
      step_dummy_multi_choice(starts_with("lang"), naming = NULL) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- languages
  rec <- recipe(~., data) |>
    step_dummy_multi_choice(all_predictors()) |>
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
