library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(covers, package = "modeldata")
covers$rows <- 1:nrow(covers)
covers$ch_rows <- paste(1:nrow(covers))

rec <- recipe(~ description + rows + ch_rows, covers)

counts <- gregexpr(pattern = "(rock|stony)", text = covers$description)
counts <- vapply(counts, function(x) length(x[x > 0]), integer(1))
chars <- nchar(covers$description)

test_that("default options", {
  rec1 <- rec %>%
    step_count(description, pattern = "(rock|stony)") %>%
    step_count(description, pattern = "", result = "every thing") %>%
    step_count(description,
      pattern = "(rock|stony)",
      result = "pct", normalize = TRUE
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
  rec2 <- rec %>%
    step_count(description,
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
    rec %>% step_count(description, rows, pattern = "(rock|stony)")
  )
  rec2 <- rec %>% step_count(rows, pattern = "(rock|stony)")
  expect_snapshot(error = TRUE,
    prep(rec2, training = covers)
  )
})


test_that("printing", {
  rec5 <- rec %>%
    step_count(description, pattern = "(rock|stony)")
  expect_snapshot(print(rec5))
  expect_snapshot(prep(rec5))
})

test_that("empty selection prep/bake adds an NA column", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_count(rec1, pattern = "rock")

  rec2 <- prep(rec2, mtcars)

  baked2 <- bake(rec2, mtcars)

  expect_identical(baked2$rock, rep(NA_integer_, nrow(mtcars)))
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_count(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), result = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), result = character(), id = character())
  )
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_count(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  mt_tibble <- mtcars %>%
    tibble::rownames_to_column(var = "make_model")

  rec <-
    mt_tibble %>%
    recipe(mpg ~ ., data = .) %>%
    step_count(make_model, pattern = "Toyota", result = "is_toyota") %>%
    update_role(make_model, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = mt_tibble)

  expect_error(bake(rec_trained, new_data = mt_tibble[,c(-1)]),
               class = "new_data_missing_column")
})
