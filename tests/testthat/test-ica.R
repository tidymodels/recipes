library(testthat)
library(recipes)
skip_if_not_installed("modeldata")

data(biomass, package = "modeldata")
biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ][1:5, ]

rec <-
  recipe(
    HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
    data = biomass_tr
  ) |>
  step_normalize(all_predictors())

# From directly calling fastICA
exp_comp <-
  structure(
    c(
      0.129863464905676,
      0.0759034631963958,
      0.345979256585598,
      0.126579800253201,
      0.156772622077837,
      0.397465725824992,
      0.697584509101097,
      0.783403578028365,
      0.838717872383214,
      -1.22958413032623,
      0.274166588383082,
      -0.429295233592478
    ),
    .Dim = c(6L, 2L),
    .Dimnames = list(NULL, c("IC1", "IC2"))
  )

exp_load <-
  structure(
    c(
      -0.41723456724402,
      0.364056809629112,
      0.406544129493068,
      0.132202024893928,
      -0.0815957464153714,
      0.192529689854096,
      -0.0343644772231862,
      0.139740384225697,
      -0.656590943491334,
      -0.519748239614668
    ),
    .Dim = c(5L, 2L)
  )
colnames(exp_load) <- c("IC1", "IC2")

test_that("correct ICA values", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  ica_extract <- rec |>
    step_ica(
      carbon,
      hydrogen,
      oxygen,
      nitrogen,
      sulfur,
      num_comp = 2,
      seed = 1,
      id = ""
    )

  set.seed(12)
  ica_extract_trained <- prep(
    ica_extract,
    training = biomass_tr,
    verbose = FALSE
  )

  ica_pred <- bake(ica_extract_trained, new_data = NULL, all_predictors())
  ica_pred <- head(as.matrix(ica_pred))

  rownames(ica_pred) <- NULL

  expect_equal(ica_pred, exp_comp)

  vars <- c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur")
  tidy_exp_un <- tibble(
    terms = rep(vars, 2),
    component = rep(c("IC1", "IC2"), 5),
    value = rep(NA_real_, 2 * 5),
    id = ""
  ) |>
    arrange(terms, component)
  expect_equal(tidy_exp_un, tidy(ica_extract, number = 2))

  loadings <- stack(as.data.frame(exp_load))

  tidy_exp_tr <- tibble(
    terms = rep(vars, 2),
    component = as.character(loadings$ind),
    value = loadings$values,
    id = ""
  ) |>
    arrange(terms, component)
  expect_equal(tidy_exp_tr, tidy(ica_extract_trained, number = 2))
})

test_that("No ICA comps", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")
  ica_extract <- rec |>
    step_ica(carbon, hydrogen, oxygen, nitrogen, sulfur, num_comp = 0)

  ica_extract_trained <- prep(ica_extract, training = biomass_tr)
  expect_equal(
    names(bake(ica_extract_trained, new_data = NULL)),
    names(biomass_tr)[-(1:2)]
  )
  expect_true(all(names(ica_extract_trained$steps[[1]]$res) == "x_vars"))
  expect_snapshot(print(ica_extract_trained))
  expect_true(all(is.na(tidy(ica_extract_trained, 2)$value)))
})

test_that("check_name() is used", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")
  dat <- mtcars
  dat$IC1 <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_ica(mpg, disp)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")
  rec <-
    recipe(~., data = iris) |>
    step_ica(all_predictors())
  rec_param <- tunable.step_ica(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("Do nothing for num_comps = 0 and keep_original_cols = FALSE (#1152)", {
  rec <- recipe(~., data = mtcars) |>
    step_ica(all_predictors(), num_comp = 0, keep_original_cols = FALSE) |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_identical(res, tibble::as_tibble(mtcars))
})

test_that("rethrows error correctly from implementation", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  local_mocked_bindings(
    .package = "fastICA",
    fastICA = function(...) {
      cli::cli_abort("mocked error")
    }
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_ica(all_predictors()) |>
      prep()
  )
})

test_that("check_options() is used", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_ica(all_predictors(), options = TRUE) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  ica_extract <-
    recipe(
      HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
      data = biomass_tr
    ) |>
    step_ica(carbon, hydrogen, num_comp = 2, seed = 1) |>
    update_role(carbon, hydrogen, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  ica_extract_trained <- prep(
    ica_extract,
    training = biomass_tr,
    verbose = FALSE
  )

  expect_snapshot(
    error = TRUE,
    bake(ica_extract_trained, new_data = biomass_tr[, c(-3)])
  )
})

test_that("empty printing", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ica(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_ica(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ica(rec)

  expect <- tibble(
    terms = character(),
    component = character(),
    value = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  new_names <- c("IC1", "IC2", "IC3", "IC4", "IC5")

  rec <- recipe(~., mtcars) |>
    step_ica(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~., mtcars) |>
    step_ica(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c(colnames(mtcars), new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  rec <- recipe(~., mtcars) |>
    step_ica(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = mtcars)
  )
})

test_that("printing", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  rec <- recipe(
    HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
    data = biomass_tr
  ) |>
    step_normalize(all_predictors()) |>
    step_ica(carbon, hydrogen, num_comp = 2)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")
  rec <- recipe(~., data = mtcars) |>
    step_ica(
      all_predictors(),
      num_comp = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("bad args", {
  skip_if_not_installed("fastICA")

  expect_snapshot(
    rec |>
      step_ica(carbon, hydrogen, prefix = 2) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  data <- mtcars
  rec <- recipe(~., data) |>
    step_ica(disp, mpg) |>
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
