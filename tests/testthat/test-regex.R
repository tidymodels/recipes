library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(covers, package = "modeldata")
covers$rows <- 1:nrow(covers)
covers$ch_rows <- paste(1:nrow(covers))

rec <- recipe(~ description + rows + ch_rows, covers)

test_that("default options", {
  rec1 <- rec %>%
    step_regex(description, pattern = "(rock|stony)") %>%
    step_regex(description, result = "all ones")
  rec1 <- prep(rec1, training = covers)
  res1 <- bake(rec1, new_data = covers)
  expect_equal(
    res1$X.rock.stony.,
    as.numeric(grepl("(rock|stony)", covers$description))
  )
  expect_equal(res1$`all ones`, rep(1, nrow(covers)))
  expect_true(is.integer(res1$X.rock.stony.))
  expect_true(is.integer(res1$`all ones`))
})


test_that("nondefault options", {
  rec2 <- rec %>%
    step_regex(description,
      pattern = "(rock|stony)",
      result = "rocks",
      options = list(fixed = TRUE)
    )
  rec2 <- prep(rec2, training = covers)
  res2 <- bake(rec2, new_data = covers)
  expect_equal(res2$rocks, rep(0, nrow(covers)))
})


test_that("bad selector(s)", {
  expect_snapshot(error = TRUE,
    rec %>% step_regex(description, rows, pattern = "(rock|stony)")
  )
  rec4 <- rec %>% step_regex(rows, pattern = "(rock|stony)")
  expect_snapshot(error = TRUE,
    prep(rec4, training = covers)
  )
})

test_that("check_name() is used", {
  dat <- iris

  rec <- recipe(~., data = dat) |>
    step_regex(Species, result = "Sepal.Width")

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  mt_tibble <- mtcars %>%
    tibble::rownames_to_column(var = "make_model")

  rec <-
    recipe(mpg ~ ., data = mt_tibble) %>%
    step_regex(make_model, pattern = "Toyota", result = "is_toyota") %>%
    update_role(make_model, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE) %>%
    prep(mt_tibble)

  expect_error(bake(rec, new_data = mt_tibble[, c(-1)]),
               class = "new_data_missing_column")
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_regex(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_regex(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_regex(rec)

  expect <- tibble(terms = character(), result = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("rocks")

  rec <-  recipe(~ description, covers) %>%
    step_regex(description, pattern = "(rock|stony)", result = "rocks",
               keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~ description, covers) %>%
    step_regex(description, pattern = "(rock|stony)", result = "rocks",
               keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("description", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~ description, covers) %>%
    step_regex(description, pattern = "(rock|stony)")

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_error(
    bake(rec, new_data = covers),
    NA
  )
})

test_that("printing", {
  rec <- recipe(~ description + rows + ch_rows, covers) %>%
    step_regex(description, pattern = "(rock|stony)")

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
