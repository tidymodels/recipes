library(recipes)
library(testthat)

test_that("simple skip", {
  rec_1 <- recipe(Sepal.Length ~ ., data = iris) |>
    step_log(Sepal.Length, skip = TRUE) |>
    step_dummy(Species) |>
    step_center(all_predictors())

  prepped_1 <- prep(rec_1, training = iris)

  juiced_1 <- juice(prepped_1)
  baked_1 <- bake(prepped_1, new_data = iris)

  expect_equal(baked_1$Sepal.Length, iris$Sepal.Length)
  expect_equal(juiced_1$Sepal.Length, log(iris$Sepal.Length))

  expect_snapshot(
    prepped_2 <- prep(rec_1, training = iris, retain = FALSE)
  )

  baked_2 <- bake(prepped_2, new_data = iris[, -1])
  baked_3 <- bake(prepped_2, new_data = iris)
  expect_false(
    isTRUE(
      all.equal(juiced_1$Sepal.Width, baked_3$Sepal.Length)
    )
  )
  expect_equal(log(baked_1$Sepal.Length), juiced_1$Sepal.Length)
  expect_equal(setdiff(names(baked_1), names(baked_2)), "Sepal.Length")
  expect_equal(setdiff(names(baked_2), names(baked_3)), character(0))

  expect_snapshot(prep(rec_1, training = iris, retain = FALSE))
})

test_that("check existing steps for `skip` arg", {
  step_check <- grep(
    pattern = "(^step_)|(^check_)",
    x = names(asNamespace("recipes")),
    value = TRUE
  )
  # These ones are not operations
  step_check <- step_check[step_check != "check_type"]
  step_check <- step_check[step_check != "check_nominal_type"]
  step_check <- step_check[step_check != "check_factor_vars"]
  step_check <- step_check[step_check != "check_name"]
  step_check <- step_check[step_check != "check_is_lat_lon"]
  step_check <- step_check[step_check != "check_new_data"]
  step_check <- step_check[step_check != "check_role_requirements"]
  step_check <- step_check[step_check != "check_bake_role_requirements"]
  step_check <- step_check[step_check != "check_step_check_args"]
  step_check <- step_check[step_check != "check_sparse_arg"]
  step_check <- step_check[step_check != "check_contrasts_arg"]
  step_check <- step_check[step_check != "check_zv"]
  step_check <- step_check[step_check != "check_options"]

  # R/import-standalone-types-check.R
  step_check <- step_check[step_check != "check_bool"]
  step_check <- step_check[step_check != "check_string"]
  step_check <- step_check[step_check != "check_name"]
  step_check <- step_check[step_check != "check_number_decimal"]
  step_check <- step_check[step_check != "check_number_whole"]
  step_check <- step_check[step_check != "check_symbol"]
  step_check <- step_check[step_check != "check_arg"]
  step_check <- step_check[step_check != "check_call"]
  step_check <- step_check[step_check != "check_environment"]
  step_check <- step_check[step_check != "check_function"]
  step_check <- step_check[step_check != "check_closure"]
  step_check <- step_check[step_check != "check_formula"]
  step_check <- step_check[step_check != "check_character"]
  step_check <- step_check[step_check != "check_logical"]
  step_check <- step_check[step_check != "check_data_frame"]

  has_skip_arg <- function(x) {
    x_code <- getFromNamespace(x, "recipes")
    x_args <- names(formals(x_code))
    "skip" %in% x_args
  }
  has_skip <- vapply(step_check, has_skip_arg, logical(1))
  if (any(!has_skip)) {
    print(names(has_skip)[!has_skip])
  }

  for (i in names(has_skip)) {
    expect_true(has_skip[i])
  }
})

test_that("skips for steps that remove columns (#239)", {
  simple_ex <-
    recipe(Species ~ ., data = iris) |>
    step_interact(terms = ~ Sepal.Length:Sepal.Width) |>
    step_rm(Sepal.Length, skip = TRUE)

  prep_simple <- prep(simple_ex, iris)
  simple_juiced <- juice(prep_simple)
  simple_baked <- bake(prep_simple, new_data = iris)

  expect_equal(
    names(simple_juiced),
    c(
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "Sepal.Length_x_Sepal.Width"
    )
  )

  # Ordering relative to the original specification of the recipe is
  # preserved (i.e. Sepal.Length was the first column specified, and since we
  # skipped the step that would drop it, it appears first in the result)
  expect_equal(
    names(simple_baked),
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "Sepal.Length_x_Sepal.Width"
    )
  )

  complex_ex <-
    recipe(Species ~ ., data = iris) |>
    step_interact(terms = ~ Sepal.Length:Sepal.Width) |>
    step_rm(Sepal.Length) |>
    step_pca(contains("Sepal")) |>
    step_rm(PC1, skip = TRUE) |>
    prep()

  complex_juiced <- juice(complex_ex)
  complex_baked <- bake(complex_ex, new_data = iris)

  expect_equal(
    names(complex_juiced),
    c("Petal.Length", "Petal.Width", "Species", "PC2")
  )
  expect_equal(
    names(complex_baked),
    c("Petal.Length", "Petal.Width", "Species", "PC1", "PC2")
  )

  iris_dups <-
    iris |>
    mutate(
      dup_1 = Sepal.Width,
      dup_2 = Sepal.Width
    )

  corr_example <-
    recipe(Species ~ ., data = iris_dups) |>
    step_corr(all_predictors(), skip = TRUE) |>
    prep()

  corr_juiced <- juice(corr_example)
  corr_baked <- bake(corr_example, new_data = iris_dups)

  expect_equal(
    names(corr_juiced),
    c("Sepal.Length", "Petal.Width", "dup_2", "Species")
  )

  expect_equal(
    names(corr_baked),
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "dup_1",
      "dup_2",
      "Species"
    )
  )

  expect_equal(
    sort(names(corr_baked)),
    sort(names(iris_dups))
  )
})
