library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(covers, package = "modeldata")
covers$rows <- 1:nrow(covers)
covers$ch_rows <- paste(1:nrow(covers))

rec <- recipe(~ description + rows + ch_rows, covers)

count_pattern <- function(text, pattern) {
  matches <- gregexpr(pattern = pattern, text = text)
  vapply(matches, function(x) length(x[x > 0]), integer(1))
}

counts <- count_pattern(covers$description, "(rock|stony)")
chars <- nchar(covers$description)

test_that("default options", {
  rec1 <- rec |>
    step_count(description, pattern = "(rock|stony)") |>
    step_count(description, pattern = "", result = "every thing") |>
    step_count(
      description,
      pattern = "(rock|stony)",
      result = "pct",
      normalize = TRUE
    )
  rec1 <- prep(rec1, training = covers)
  res1 <- bake(rec1, new_data = covers)
  expect_equal(res1$X.rock.stony., counts)
  expect_equal(res1$`every thing`, chars)
  expect_equal(res1$pct, counts / chars)

  expect_true(is.integer(res1$X.rock.stony.))
  expect_true(is.integer(res1$`every thing`))
  expect_false(is.integer(res1$pct))
})

test_that("nondefault options", {
  rec2 <- rec |>
    step_count(
      description,
      pattern = "(rock|stony)",
      result = "rocks",
      options = list(fixed = TRUE)
    )
  rec2 <- prep(rec2, training = covers)
  res2 <- bake(rec2, new_data = covers)
  expect_equal(res2$rocks, rep(0, nrow(covers)))
})

test_that("bad selector(s)", {
  rec2 <- rec |> step_count(rows, pattern = "(rock|stony)")
  expect_snapshot(error = TRUE, prep(rec2, training = covers))
})

test_that("multiple selections work", {
  res <- rec |>
    step_count(description, ch_rows, pattern = "1", result = "ones") |>
    prep(training = covers) |>
    bake(new_data = covers)

  expect_equal(res$description_ones, count_pattern(covers$description, "1"))
  expect_equal(res$ch_rows_ones, count_pattern(covers$ch_rows, "1"))
})

test_that("a single selection is named with `result` alone", {
  res <- rec |>
    step_count(description, pattern = "1", result = "ones") |>
    prep(training = covers) |>
    bake(new_data = covers)

  expect_equal(res$ones, count_pattern(covers$description, "1"))
})

test_that("a selector resolving to multiple columns works", {
  res <- recipe(~ description + ch_rows, covers) |>
    step_count(all_string_predictors(), pattern = "1", result = "ones") |>
    prep(training = covers) |>
    bake(new_data = covers)

  expect_named(
    res,
    c("description", "ch_rows", "description_ones", "ch_rows_ones")
  )
})

test_that("normalize works with multiple selections", {
  res <- rec |>
    step_count(
      description,
      ch_rows,
      pattern = "1",
      result = "ones",
      normalize = TRUE
    ) |>
    prep(training = covers) |>
    bake(new_data = covers)

  expect_equal(
    res$description_ones,
    count_pattern(covers$description, "1") / nchar(covers$description)
  )
  expect_equal(
    res$ch_rows_ones,
    count_pattern(covers$ch_rows, "1") / nchar(covers$ch_rows)
  )
})

test_that("tidy method works with multiple selections", {
  rec1 <- rec |>
    step_count(description, ch_rows, pattern = "1", result = "ones") |>
    prep(training = covers)

  expect_identical(
    tidy(rec1, number = 1),
    tibble(
      terms = c("description", "ch_rows"),
      result = c("description_ones", "ch_rows_ones"),
      id = rec1$steps[[1]]$id
    )
  )
})

test_that("keep_original_cols works with multiple selections", {
  res <- rec |>
    step_count(
      description,
      ch_rows,
      pattern = "1",
      result = "ones",
      keep_original_cols = FALSE
    ) |>
    prep(training = covers) |>
    bake(new_data = covers)

  expect_named(res, c("rows", "description_ones", "ch_rows_ones"))
})

test_that("sparse = 'yes' works with multiple selections", {
  rec1 <- recipe(~ description + ch_rows, covers)

  suppressWarnings({
    dense <- rec1 |>
      step_count(
        description,
        ch_rows,
        pattern = "stony",
        sparse = "no",
        keep_original_cols = FALSE
      ) |>
      prep() |>
      bake(NULL)
    sparse <- rec1 |>
      step_count(
        description,
        ch_rows,
        pattern = "stony",
        sparse = "yes",
        keep_original_cols = FALSE
      ) |>
      prep() |>
      bake(NULL)
  })

  expect_identical(dense, sparse)
  expect_all_true(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1)))
})

test_that("check_name() is used", {
  dat <- iris

  rec <- recipe(~., data = dat) |>
    step_count(Species, result = "Sepal.Width")

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("checks for grepl arguments", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_count(options = list(not_real_option = TRUE))
  )
})

test_that("sparse = 'yes' works", {
  rec <- recipe(~description, covers)

  suppressWarnings({
    dense <- rec |>
      step_count(
        description,
        pattern = "stony",
        sparse = "no",
        keep_original_cols = FALSE
      ) |>
      prep() |>
      bake(NULL)
    sparse <- rec |>
      step_count(
        description,
        pattern = "stony",
        sparse = "yes",
        keep_original_cols = FALSE
      ) |>
      prep() |>
      bake(NULL)
  })

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_vector, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  rec <- recipe(~description, covers) |>
    step_count(description, pattern = "stony") |>
    prep()

  exp <- bake(rec, covers)

  # Simulate old recipe
  rec$steps[[1]]$sparse <- NULL

  expect_identical(
    bake(rec, covers),
    exp
  )
})

test_that(".recipes_toggle_sparse_args works", {
  rec <- recipe(~description, covers) |>
    step_count(description, pattern = "stony", sparse = "auto")

  exp <- rec |> prep() |> bake(NULL) |> sparsevctrs::sparsity()

  expect_true(
    .recipes_estimate_sparsity(rec) > exp
  )
})

test_that("check_options() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(~description, data = covers) |>
      step_count(description, options = TRUE) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  mt_tibble <- mtcars |>
    tibble::rownames_to_column(var = "make_model")

  rec <- recipe(mpg ~ ., data = mt_tibble) |>
    step_count(make_model, pattern = "Toyota", result = "is_toyota") |>
    update_role(make_model, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = mt_tibble)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = mt_tibble[, c(-1)])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_count(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_count(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_count(rec)

  expect <- tibble(terms = character(), result = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("rocks")

  rec <- recipe(~description, covers) |>
    step_count(
      description,
      pattern = "(rock|stony)",
      result = "rocks",
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~description, covers) |>
    step_count(
      description,
      pattern = "(rock|stony)",
      result = "rocks",
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("description", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~description, covers) |>
    step_count(
      description,
      pattern = "(rock|stony)",
      result = "rocks",
      keep_original_cols = FALSE
    )

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = covers)
  )
})

test_that("printing", {
  rec <- rec |>
    step_count(description, pattern = "(rock|stony)")

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  skip_if_not_installed("modeldata")
  data(covers, package = "modeldata")

  expect_snapshot(
    recipe(~description, covers) |>
      step_count(description, pattern = character(0)) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~description, covers) |>
      step_count(description, pattern = "(rock|stony)", result = letters) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~description, covers) |>
      step_count(description, pattern = "(rock|stony)", normalize = "yes") |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- covers
  rec <- recipe(~., data) |>
    step_count(description, pattern = "(rock|stony)") |>
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
