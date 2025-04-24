library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(tate_text, package = "modeldata")

color_examples <- tibble(
  colors = c(
    "['red', 'blue']",
    "['red', 'blue', 'white']",
    "['blue', 'blue', 'blue']"
  )
)

color_result <- tribble(
  ~colors_blue,
  ~colors_red,
  ~colors_white,
  ~colors_other,
  1L,
  1L,
  0L,
  0L,
  1L,
  1L,
  1L,
  0L,
  3L,
  0L,
  0L,
  0L
)

mini_tate <- tate_text[c(101, 102, 105, 108), ]

mini_tate_result <- tibble(
  medium_Charcoal = c(0L, 0L, 0L, 1L),
  medium_Etching = c(1L, 1L, 1L, 0L),
  medium_aquatint = c(1L, 0L, 0L, 0L),
  medium_gouache = c(0L, 0L, 0L, 1L),
  medium_paper = c(1L, 1L, 1L, 1L),
  medium_other = c(0L, 0L, 0L, 0L)
)

test_that("dummy variables", {
  # Using `sep` argument
  dummy <- recipe(~medium, data = mini_tate) |>
    step_dummy_extract(medium, sep = "( and )|( on )", id = "")

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = mini_tate)

  expect_identical(dummy_pred, mini_tate_result)
  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "medium",
      columns = c("paper", "Etching", "Charcoal", "aquatint", "gouache"),
      id = ""
    )
  )

  # Using `pattern` argument
  dummy <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", id = "")

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(dummy_pred, color_result)
  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:3],
      id = ""
    )
  )
})

test_that("other argument", {
  # Using `sep` argument
  dummy <- recipe(~medium, data = mini_tate) |>
    step_dummy_extract(medium, sep = "( and )|( on )", id = "", other = "cake")

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = mini_tate)

  expect_identical(names(dummy_pred)[length(dummy_pred)], "medium_cake")
})

test_that("error when neither sep or pattern is specified", {
  expect_snapshot(
    error = TRUE,
    recipe(~medium, data = tate_text) |>
      step_dummy_extract(medium) |>
      prep()
  )
})

test_that("dummy variables with threshold", {
  # threshold = 0.5
  dummy <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(
      colors,
      pattern = "(?<=')[^',]+(?=')",
      id = "",
      threshold = 0.5
    )

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result |>
      mutate(colors_other = colors_other + colors_white) |>
      select(-colors_white)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:2],
      id = ""
    )
  )

  # threshold = 0.8
  dummy <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(
      colors,
      pattern = "(?<=')[^',]+(?=')",
      id = "",
      threshold = 0.8
    )

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result |>
      mutate(colors_other = colors_other + colors_white + colors_red) |>
      select(-colors_white, -colors_red)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1],
      id = ""
    )
  )
})

test_that("dummy variables with integer threshold", {
  # threshold = 1
  dummy <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(
      colors,
      pattern = "(?<=')[^',]+(?=')",
      id = "",
      threshold = 1
    )

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:3],
      id = ""
    )
  )

  # threshold = 2
  dummy <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(
      colors,
      pattern = "(?<=')[^',]+(?=')",
      id = "",
      threshold = 2
    )

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result |>
      mutate(colors_other = colors_other + colors_white) |>
      select(-colors_white)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:2],
      id = ""
    )
  )

  # threshold = 3
  dummy <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(
      colors,
      pattern = "(?<=')[^',]+(?=')",
      id = "",
      threshold = 3
    )

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result |>
      mutate(colors_other = colors_other + colors_white + colors_red) |>
      select(-colors_white, -colors_red)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1],
      id = ""
    )
  )
})

test_that("check_name() is used", {
  dat <- iris
  dat$Species_setosa <- dat$Species

  rec <- recipe(~., data = dat) |>
    step_dummy_extract(Species, sep = " ")

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("naming function", {
  expect_equal(
    dummy_extract_names("x", letters[c(1, 2, 3, 3)]),
    c("x_a", "x_b", "x_c", "x_c_2")
  )
  expect_equal(
    dummy_extract_names("x", letters[c(1, 2, 3, 3)], ordinal = TRUE),
    c("x_1", "x_2", "x_3", "x_4")
  )
})

test_that("case weights", {
  mini_tate_cw <- mini_tate |>
    mutate(wts = frequency_weights(c(1, 1, 1, 5)))

  dummy <- recipe(~ medium + wts, data = mini_tate_cw) |>
    step_dummy_extract(medium, sep = "( and )|( on )", id = "", threshold = 6)

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = mini_tate)

  exp_results <- tibble(
    medium_paper = c(1L, 1L, 1L, 1L),
    medium_other = c(2L, 1L, 1L, 2L)
  )

  expect_identical(dummy_pred, exp_results)
  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "medium",
      columns = "paper",
      id = ""
    )
  )

  expect_snapshot(dummy_prepped)

  # importance weights are not considered
  mini_tate_cw <- mini_tate |>
    mutate(wts = importance_weights(c(1, 1, 1, 5)))

  dummy <- recipe(~ medium + wts, data = mini_tate_cw) |>
    step_dummy_extract(medium, sep = "( and )|( on )", id = "", threshold = 6)

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = mini_tate)

  exp_results <- tibble(
    medium_other = c(3L, 2L, 2L, 3L)
  )

  expect_identical(dummy_pred, exp_results)
  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = character(),
      columns = character(),
      id = character()
    )
  )

  expect_snapshot(dummy_prepped)
})

test_that("sparse = 'yes' works", {
  rec <- recipe(~medium, data = mini_tate)

  dense <- rec |>
    step_dummy_extract(medium, sep = "( and )|( on )", sparse = "no") |>
    prep() |>
    bake(NULL)
  dense <- purrr::map(dense, as.integer) |> tibble::new_tibble()
  sparse <- rec |>
    step_dummy_extract(medium, sep = "( and )|( on )", sparse = "yes") |>
    prep() |>
    bake(NULL)

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_vector, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  rec <- recipe(~medium, data = mini_tate) |>
    step_dummy_extract(medium, sep = "( and )|( on )") |>
    prep()

  exp <- bake(rec, mini_tate)

  # Simulate old recipe
  rec$steps[[1]]$sparse <- NULL

  expect_identical(
    bake(rec, mini_tate),
    exp
  )
})

test_that(".recipes_toggle_sparse_args works", {
  rec <- recipe(~medium, data = mini_tate) |>
    step_dummy_extract(medium, sep = "( and )|( on )", sparse = "auto")

  exp <- rec |> prep() |> bake(NULL) |> sparsevctrs::sparsity()

  expect_true(
    .recipes_estimate_sparsity(rec) > exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  dummy <- recipe(~medium, data = mini_tate) |>
    step_dummy_extract(medium, sep = "( and )|( on )", id = "") |>
    update_role(medium, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  dummy_prepped <- prep(dummy)

  expect_snapshot(
    error = TRUE,
    bake(dummy_prepped, new_data = mini_tate[, 1:3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_extract(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_dummy_extract(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_extract(rec)

  expect <- tibble(terms = character(), columns = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- paste0("colors_", c("blue", "red", "white", "other"))

  rec <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(
      colors,
      pattern = "(?<=')[^',]+(?=')",
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(
      colors,
      pattern = "(?<=')[^',]+(?=')",
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("colors", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~colors, data = color_examples) |>
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')")

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = color_examples)
  )
})

test_that("printing", {
  rec <- recipe(~medium, data = tate_text) |>
    step_dummy_extract(all_predictors(), sep = ", ")

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(~colors, data = color_examples) |>
      step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", other = 2) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~colors, data = color_examples) |>
      step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", other = 2) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~colors, data = color_examples) |>
      step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", sep = 2) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~colors, data = color_examples) |>
      step_dummy_extract(colors, pattern = 2) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~colors, data = color_examples) |>
      step_dummy_extract(
        colors,
        pattern = "(?<=')[^',]+(?=')",
        naming = NULL
      ) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- color_examples
  rec <- recipe(~., data) |>
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')") |>
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
