library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data("Sacramento", package = "modeldata")

sacr <- Sacramento

sacr$city <- as.character(sacr$city)
sacr$zip <- as.character(sacr$zip)

set.seed(1)
sacr$city[sample(1:nrow(sacr), 20)] <- NA_character_

sacr_missing <- sacr

sacr$city[is.na(sacr$city)] <- "missing"
sacr <- sacr[vec_detect_complete(sacr), -3]

sacr_fac <- sacr
sacr_fac$city <- factor(sacr_fac$city)
sacr_fac$zip <- factor(sacr_fac$zip)

test_that("dummy variables with factor inputs", {
  rec <- recipe(sqft ~ zip + city, data = sacr_fac, strings_as_factors = FALSE)
  dummy <- rec |> step_dummy(city, zip, id = "")
  dummy_trained <- prep(
    dummy,
    training = sacr_fac,
    verbose = FALSE
  )
  dummy_pred <- bake(dummy_trained, new_data = sacr_fac, all_predictors())

  expect_false(any(colnames(dummy_pred) == "city"))
  expect_false(any(colnames(dummy_pred) == "zip"))

  dummy_pred <- dummy_pred[, order(colnames(dummy_pred))]
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  exp_res <- model.matrix(sqft ~ zip + city, data = sacr_fac)[, -1]
  exp_res <- exp_res[, colnames(exp_res) != "sqft"]
  colnames(exp_res) <- gsub("^zip", "zip_", colnames(exp_res))
  colnames(exp_res) <- gsub("^city", "city_", colnames(exp_res))
  colnames(exp_res) <- make.names(colnames(exp_res))
  exp_res <- exp_res[, order(colnames(exp_res))]
  exp_res <- as.data.frame(exp_res)
  rownames(exp_res) <- NULL
  expect_equal(dummy_pred, exp_res, ignore_attr = TRUE)

  dum_tibble <-
    tibble(terms = c("city", "zip"), columns = rep(rlang::na_chr, 2), id = "")
  dum_tibble_prepped_1 <-
    tibble(
      terms = "city",
      columns = attributes(dummy_trained$steps[[1]]$levels$city)$values,
      id = ""
    ) |>
    slice(-1)
  dum_tibble_prepped_2 <-
    tibble(
      terms = "zip",
      columns = attributes(dummy_trained$steps[[1]]$levels$zip)$values,
      id = ""
    ) |>
    slice(-1)
  expect_equal(tidy(dummy, 1), dum_tibble)
  expect_equal(
    tidy(dummy_trained, 1),
    bind_rows(dum_tibble_prepped_1, dum_tibble_prepped_2)
  )
})

test_that("dummy variables errors with character inputs", {
  rec <- recipe(sqft ~ zip + city, data = sacr, strings_as_factors = FALSE)
  dummy <- rec |> step_dummy(city, zip)

  expect_snapshot(
    error = TRUE,
    prep(dummy, training = sacr, verbose = FALSE)
  )
})

test_that("check_type() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(sqft ~ zip + price + city, data = sacr) |>
      step_dummy(city, zip, price) |>
      prep()
  )
})

test_that("create double dummy variables", {
  rec <- recipe(sqft ~ zip + city, data = sacr_fac, strings_as_factors = FALSE)
  dummy <- rec |> step_dummy(city, zip, id = "")
  dummy_trained <- prep(
    dummy,
    training = sacr_fac,
    verbose = FALSE
  )
  dummy_pred <- bake(dummy_trained, new_data = sacr_fac, all_predictors())
  expect_true(all(vapply(dummy_pred, is.double, logical(1))))
})

test_that("create all dummy variables", {
  rec <- recipe(
    sqft ~ zip + city + price,
    data = sacr_fac,
    strings_as_factors = FALSE
  )
  dummy <- rec |> step_dummy(city, zip, one_hot = TRUE, id = "")
  dummy_trained <- prep(
    dummy,
    training = sacr_fac,
    verbose = FALSE
  )
  dummy_pred <- bake(dummy_trained, new_data = sacr_fac, all_predictors())
  dummy_pred <- dummy_pred[, order(colnames(dummy_pred))]
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  exp_res <- NULL
  for (pred in c("city", "price", "zip")) {
    tmp <- model.matrix(as.formula(paste("~", pred, "+ 0")), data = sacr_fac)
    colnames(tmp) <- gsub(paste0("^", pred), paste0(pred, "_"), colnames(tmp))
    exp_res <- bind_cols(exp_res, as_tibble(tmp))
  }
  colnames(exp_res) <- make.names(colnames(exp_res))
  exp_res <- as.data.frame(exp_res)
  rownames(exp_res) <- NULL
  # TODO: need some help with this one
  # expect_equal(dummy_pred, exp_res, ignore_attr = TRUE)

  dum_tibble <-
    tibble(terms = c("city", "zip"), columns = rep(rlang::na_chr, 2), id = "")
  dum_tibble_prepped_1 <-
    tibble(
      terms = "city",
      columns = attributes(dummy_trained$steps[[1]]$levels$city)$values,
      id = ""
    )
  dum_tibble_prepped_2 <-
    tibble(
      terms = "zip",
      columns = attributes(dummy_trained$steps[[1]]$levels$zip)$values,
      id = ""
    )
  expect_equal(
    tidy(dummy_trained, 1),
    bind_rows(dum_tibble_prepped_1, dum_tibble_prepped_2)
  )
})

test_that("make sure contrasts argument work", {
  rec <- recipe(
    ~city,
    data = sacr_fac,
    strings_as_factors = FALSE
  )
  dummy <- rec |> step_dummy(city, contrasts = "contr.poly", id = "")
  dummy_trained <- prep(
    dummy,
    training = sacr_fac,
    verbose = FALSE
  )
  dummy_pred <- bake(dummy_trained, new_data = sacr_fac)

  pred <- "city"
  tmp <- model.matrix(
    as.formula(paste("~", pred, "+ 0")),
    data = sacr_fac,
    contrasts.arg = setNames(list(contr.poly), pred)
  )
  exp_res <- as_tibble(tmp %*% attr(tmp, "contrasts")[[pred]])

  expect_identical(unname(dummy_pred), unname(exp_res))
})

test_that("make sure contrasts argument work for ordered factors", {
  sacr_fac$city <- as.ordered(sacr_fac$city)
  rec <- recipe(
    ~city,
    data = sacr_fac,
    strings_as_factors = FALSE
  )
  dummy <- rec |>
    step_dummy(
      city,
      one_hot = TRUE,
      contrasts = list(
        unordered = "contr.poly",
        ordered = "contr.treatment"
      ),
      id = ""
    )
  dummy_trained <- prep(
    dummy,
    training = sacr_fac,
    verbose = FALSE
  )
  dummy_pred <- bake(dummy_trained, new_data = sacr_fac)
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  pred <- "city"
  tmp <- model.matrix(
    as.formula(paste("~", pred, "+ 0")),
    data = sacr_fac,
    contrasts.arg = setNames(list("contr.treatment"), pred)
  )
  exp_res <- as.data.frame(tmp)
  rownames(exp_res) <- NULL

  expect_identical(unname(dummy_pred), unname(exp_res))
})

test_that("make sure contrasts argument work non-base contrasts", {
  library(hardhat)
  rec <- recipe(
    ~city,
    data = sacr_fac,
    strings_as_factors = FALSE
  )
  dummy <- rec |> step_dummy(city, contrasts = "contr_one_hot", id = "")
  dummy_trained <- prep(
    dummy,
    training = sacr_fac,
    verbose = FALSE
  )
  dummy_pred <- bake(dummy_trained, new_data = sacr_fac)
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  pred <- "city"
  tmp <- model.matrix(
    as.formula(paste("~", pred, "+ 0")),
    data = sacr_fac,
    contrasts.arg = setNames(list(contr_one_hot), pred)
  )
  exp_res <- as.data.frame(tmp)
  rownames(exp_res) <- NULL

  expect_identical(unname(dummy_pred), unname(exp_res))
})

test_that("make sure contrasts argument is checked", {
  expect_snapshot(
    error = TRUE,
    recipe(~Species, iris) |>
      step_dummy(Species, contrasts = TRUE) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~Species, iris) |>
      step_dummy(Species, contrasts = list()) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~Species, iris) |>
      step_dummy(Species, contrasts = list(ordered = "contr.treatment")) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~Species, iris) |>
      step_dummy(
        Species,
        contrasts = list(ordered = 1, unordered = "contr.treatment")
      ) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~Species, iris) |>
      step_dummy(
        Species,
        contrasts = list(ordered = "contr.treatment", unordered = 1)
      ) |>
      prep()
  )
})

test_that("getOption('contrasts') gives deprecation warning in step_dummy", {
  param <- getOption("contrasts")

  go_helmert <- param
  go_helmert["unordered"] <- "contr.helmert"
  withr::local_options("contrasts" = go_helmert)

  expect_snapshot(
    tmp <- recipe(~., data = iris) |>
      step_dummy(Species) |>
      prep()
  )
})

test_that("backwards compatible for contrasts", {
  rec <- recipe(~., data = iris) |>
    step_dummy(Species) |>
    prep()

  exp <- bake(rec, iris)

  rec$steps[[1]]$contrasts <- NULL

  expect_identical(
    bake(rec, iris),
    exp
  )
})

test_that("tests for issue #91", {
  rec <- recipe(~city, data = sacr)
  factors <- rec |> step_dummy(city)
  factors <- prep(factors, training = sacr)
  factors_data_1 <- bake(factors, new_data = sacr)
  # Remove one category in city
  factors_data_2 <- bake(
    factors,
    new_data = sacr |> filter(city != "SACRAMENTO")
  )
  expect_equal(names(factors_data_1), names(factors_data_2))

  # now with ordered factor

  sacr$ordered_city <- as.ordered(sacr$city)
  rec <- recipe(~ordered_city, data = sacr)
  orderedfac <- rec |> step_dummy(ordered_city)
  orderedfac <- prep(orderedfac, training = sacr)
  ordered_data_1 <- bake(orderedfac, new_data = sacr)
  # Remove one category in city
  ordered_data_2 <- bake(
    orderedfac,
    new_data = sacr |> filter(city != "SACRAMENTO")
  )
  expect_equal(names(ordered_data_1), names(ordered_data_2))
})

test_that("tests for NA values in factor", {
  rec <- recipe(~city, data = sacr_missing)
  factors <- rec |> step_dummy(city)
  expect_snapshot(
    factors <- prep(factors, training = sacr_missing)
  )

  factors_data_0 <- bake(factors, new_data = NULL)
  expect_snapshot(
    factors_data_1 <- bake(factors, new_data = sacr_missing)
  )

  expect_true(
    all(
      vec_detect_complete(factors_data_0) ==
        vec_detect_complete(sacr_missing[, "city"])
    )
  )
  expect_true(
    all(
      vec_detect_complete(factors_data_1) ==
        vec_detect_complete(sacr_missing[, "city"])
    )
  )
})

test_that("tests for NA values in ordered factor", {
  sacr_ordered <- sacr_missing
  sacr_ordered$city <- as.ordered(sacr_ordered$city)
  rec <- recipe(~city, data = sacr_ordered)
  factors <- rec |> step_dummy(city)
  expect_snapshot(
    factors <- prep(factors, training = sacr_ordered)
  )

  factors_data_0 <- bake(factors, new_data = NULL)
  expect_snapshot(
    factors_data_1 <- bake(factors, new_data = sacr_ordered)
  )

  expect_true(
    all(
      vec_detect_complete(factors_data_0) ==
        vec_detect_complete(sacr_ordered[, "city"])
    )
  )
  expect_true(
    all(
      vec_detect_complete(factors_data_1) ==
        vec_detect_complete(sacr_ordered[, "city"])
    )
  )
})

test_that("new levels", {
  df <- data.frame(
    y = c(1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0),
    x1 = c(
      "A",
      "B",
      "B",
      "B",
      "B",
      "A",
      "A",
      "A",
      "B",
      "A",
      "A",
      "B",
      "A",
      "C",
      "C",
      "B",
      "A",
      "B",
      "C",
      "A"
    ),
    stringsAsFactors = FALSE
  )
  training <- df[1:10, ]
  testing <- df[11:20, ]
  training$y <- as.factor(training$y)
  training$x1 <- as.factor(training$x1)
  testing$y <- as.factor(testing$y)
  testing$x1 <- as.factor(testing$x1)

  expect_snapshot(
    recipes:::warn_new_levels(
      testing$x1,
      levels(training$x1),
      "column",
      "step_dummy"
    )
  )
  expect_silent(
    recipes:::warn_new_levels(
      training$x1,
      levels(training$x1),
      "column",
      "step_dummy"
    )
  )

  rec <- recipe(y ~ x1, data = training) |>
    step_dummy(x1)
  expect_silent(
    rec <- prep(rec, training = training)
  )
  expect_snapshot(
    bake(rec, new_data = testing)
  )
})

test_that("warns about NA in column (#450)", {
  data <- data.frame(a = c(LETTERS, NA))

  expect_snapshot(
    tmp <- recipe(~a, data = data) |>
      step_dummy(a) |>
      prep()
  )
})

test_that("tests for issue #301", {
  rec <- recipe(~Species, data = iris)
  dummies <- rec |> step_dummy(Species)
  dummies <- prep(dummies, training = iris)
  expect_equal(NULL, attr(dummies$steps[[1]]$levels$Species, ".Environment"))

  saved_recipe <- tempfile()
  saveRDS(dummies, file = saved_recipe)
  read_recipe <- readRDS(file = saved_recipe)
  unlink(saved_recipe)
  expect_equal(
    bake(dummies, new_data = iris),
    bake(read_recipe, new_data = iris)
  )

  saved_dummies <- dummies
  saved_recipe <- tempfile()
  save(saved_dummies, file = saved_recipe)
  rm(saved_dummies)
  load(file = saved_recipe)
  unlink(saved_recipe)
  expect_equal(
    bake(dummies, new_data = iris),
    bake(saved_dummies, new_data = iris)
  )
})

test_that("works with non-standard column names", {
  df <- tibble(`with space` = factor(letters[1:3]))

  expect_equal(
    recipe(~., data = df) |>
      step_dummy(all_predictors()) |>
      prep() |>
      bake(new_data = NULL) |>
      colnames(),
    c("with space_b", "with space_c")
  )

  expect_equal(
    recipe(~., data = df) |>
      step_dummy(all_predictors(), one_hot = TRUE) |>
      prep() |>
      bake(new_data = NULL) |>
      colnames(),
    c("with space_a", "with space_b", "with space_c")
  )
})

test_that("naming function", {
  expect_equal(dummy_names("x", letters[1:3]), c("x_a", "x_b", "x_c"))
  expect_equal(
    dummy_names("x", letters[1:3], ordinal = TRUE),
    c("x_1", "x_2", "x_3")
  )
})

test_that("Deprecation warning", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_dummy(preserve = TRUE)
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
    step_dummy(all_nominal()) |>
    prep(training = zdat)

  expect_null(rec$steps[[2]]$levels)

  expect_equal(names(bake(rec, zdat)), c("z", "y"))

  expect_snapshot(print(rec))

  exp_tidy <- tibble(
    terms = character(),
    columns = character(),
    id = character()
  )
  expect_equal(exp_tidy, tidy(rec, number = 2))
})

test_that("check_name() is used", {
  dat <- iris
  dat$Species_versicolor <- dat$Species

  rec <- recipe(~., data = dat) |>
    step_dummy(Species)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("throws a informative error for too many levels (#828)", {
  skip_on_cran()
  dat <- data.frame(x = as.character(1:123456))

  rec <- recipe(~., data = dat) |>
    step_dummy(x)

  expect_snapshot(
    error = TRUE,
    prep(rec)
  )
})

test_that("throws an informative error for single level", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = data.frame(x = "only-level")) |>
      step_dummy(x) |>
      prep()
  )
})

test_that("sparse = 'yes' works", {
  rec <- recipe(~., data = tibble(x = c(NA, letters)))

  suppressWarnings({
    dense <- rec |> step_dummy(x, sparse = "no") |> prep() |> bake(NULL)
    dense <- purrr::map(dense, as.integer) |> tibble::new_tibble()
    sparse <- rec |> step_dummy(x, sparse = "yes") |> prep() |> bake(NULL)
  })

  expect_identical(dense, sparse)

  expect_false(any(vapply(dense, sparsevctrs::is_sparse_vector, logical(1))))
  expect_true(all(vapply(sparse, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse = 'yes' will go back to 'no' on unsupported contrasts", {
  rec <- recipe(~., data = tibble(x = letters)) |>
    step_dummy(x, sparse = "yes", contrasts = "contr.helmert") |>
    prep()

  res <- bake(rec, tibble(x = letters))

  expect_false(any(vapply(res, sparsevctrs::is_sparse_vector, logical(1))))
})

test_that("sparse argument is backwards compatible", {
  dat <- tibble(x = c(letters))
  rec <- recipe(~., data = dat) |>
    step_dummy(x) |>
    prep()

  exp <- bake(rec, dat)

  # Simulate old recipe
  rec$steps[[1]]$sparse <- NULL

  expect_identical(
    bake(rec, dat),
    exp
  )
})

test_that(".recipes_toggle_sparse_args works", {
  rec <- recipe(~., iris) |>
    step_dummy(all_nominal_predictors(), sparse = "auto")

  exp <- rec |> prep() |> bake(NULL) |> sparsevctrs::sparsity()

  expect_equal(
    .recipes_estimate_sparsity(rec),
    exp
  )

  iris$Species <- as.character(iris$Species)

  rec <- recipe(~., iris) |>
    step_dummy(all_nominal_predictors(), sparse = "auto")

  exp <- rec |> prep() |> bake(NULL) |> sparsevctrs::sparsity()

  expect_equal(
    .recipes_estimate_sparsity(rec),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(sqft ~ zip + city, data = sacr_fac, strings_as_factors = FALSE)
  dummy <- rec |>
    step_dummy(city, zip, id = "") |>
    update_role(city, zip, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)
  dummy_trained <- prep(
    dummy,
    training = sacr_fac,
    verbose = FALSE
  )

  expect_snapshot(
    error = TRUE,
    bake(dummy_trained, new_data = sacr_fac[, 3:4], all_predictors())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_dummy(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy(rec)

  expect <- tibble(terms = character(), columns = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("Species_versicolor", "Species_virginica")

  rec <- recipe(~Species, iris) |>
    step_dummy(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~Species, iris) |>
    step_dummy(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("Species", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~Species, iris) |>
    step_dummy(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = iris)
  )
})

test_that("printing", {
  rec <- recipe(sqft ~ ., data = sacr_fac) |>
    step_dummy(city, zip)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  skip_if_not_installed("modeldata")
  data(Sacramento, package = "modeldata")

  expect_snapshot(
    recipe(~ city + sqft + price, data = Sacramento) |>
      step_dummy(city, one_hot = 2) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~ city + sqft + price, data = Sacramento) |>
      step_dummy(city, naming = NULL) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- iris
  rec <- recipe(~., data) |>
    step_dummy(all_nominal_predictors()) |>
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
