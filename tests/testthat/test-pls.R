library(testthat)
library(recipes)

skip_if_not_installed("modeldata")

## -----------------------------------------------------------------------------

data(biomass, package = "modeldata")

biom_tr <- biomass |>
  dplyr::filter(dataset == "Training") |>
  dplyr::select(-dataset, -sample)
biom_te <- biomass |>
  dplyr::filter(dataset == "Testing") |>
  dplyr::select(-dataset, -sample, -HHV)

data(cells, package = "modeldata")

cell_tr <- cells |>
  dplyr::filter(case == "Train") |>
  dplyr::select(-case)
cell_te <- cells |>
  dplyr::filter(case == "Test") |>
  dplyr::select(-case, -class)

load(test_path("test_pls_new.RData"))

## -----------------------------------------------------------------------------

test_that("PLS, dense loadings", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) |>
    step_pls(all_predictors(), outcome = HHV, num_comp = 3)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- bake(rec, new_data = NULL, all_predictors())
  expect_equal(tr_new, bm_pls_tr)
  te_new <- bake(rec, biom_te)
  expect_equal(te_new, bm_pls_te)
})

test_that("PLS, dense loadings, multiple outcomes", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV + carbon ~ ., data = biom_tr) |>
    step_pls(all_predictors(), outcome = c("HHV", "carbon"), num_comp = 3)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- bake(rec, new_data = NULL, all_predictors())
  expect_equal(tr_new, bm_pls_multi_tr)
  te_new <- bake(rec, biom_te |> select(-carbon))
  expect_equal(te_new, bm_pls_multi_te)
})

test_that("PLS, sparse loadings", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) |>
    step_pls(
      all_predictors(),
      outcome = HHV,
      num_comp = 3,
      predictor_prop = 3 / 5
    )

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- bake(rec, new_data = NULL, all_predictors())
  expect_equal(tr_new, bm_spls_tr)
  te_new <- bake(rec, biom_te)
  expect_equal(te_new, bm_spls_te)
})

test_that("PLS, dense loadings, multiple outcomes", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV + carbon ~ ., data = biom_tr) |>
    step_pls(
      all_predictors(),
      outcome = c("HHV", "carbon"),
      num_comp = 3,
      predictor_prop = 3 / 5
    )

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- bake(rec, new_data = NULL, all_predictors())
  expect_equal(tr_new, bm_spls_multi_tr)
  te_new <- bake(rec, biom_te |> select(-carbon))
  expect_equal(te_new, bm_spls_multi_te)
})

## -----------------------------------------------------------------------------

test_that("PLS-DA, dense loadings", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(class ~ ., data = cell_tr) |>
    step_pls(all_predictors(), outcome = class, num_comp = 3)

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- bake(rec, new_data = NULL, all_predictors())
  expect_equal(tr_new, cell_plsda_tr)
  te_new <- bake(rec, cell_te)
  expect_equal(te_new, cell_plsda_te)
})

test_that("PLS-DA, dense loadings, multiple outcomes", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(class + case ~ ., data = cells) |>
    step_pls(all_predictors(), outcome = c("class", "case"), num_comp = 3)

  expect_snapshot(error = TRUE, prep(rec))
})

test_that("PLS-DA, sparse loadings", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(class ~ ., data = cell_tr) |>
    step_pls(
      all_predictors(),
      outcome = class,
      num_comp = 3,
      predictor_prop = 50 / 56
    )

  rec <- prep(rec)

  expect_equal(
    names(rec$steps[[1]]$res),
    c("mu", "sd", "coefs", "col_norms")
  )

  tr_new <- bake(rec, new_data = NULL, all_predictors())
  expect_equal(tr_new, cell_splsda_tr)
  te_new <- bake(rec, cell_te)
  expect_equal(te_new, cell_splsda_te)
})

test_that("PLS-DA, sparse loadings, multiple outcomes", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(class + case ~ ., data = cells) |>
    step_pls(
      all_predictors(),
      outcome = c("class", "case"),
      num_comp = 3,
      predictor_prop = 50 / 56
    )

  expect_snapshot(error = TRUE, prep(rec))
})

## -----------------------------------------------------------------------------

test_that("No PLS", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(class ~ ., data = cell_tr) |>
    step_pls(all_predictors(), outcome = class, num_comp = 0)

  rec <- prep(rec)

  expect_null(
    rec$steps[[1]]$res
  )
  pred_names <- summary(rec)$variable[summary(rec)$role == "predictor"]

  tr_new <- bake(rec, new_data = NULL, all_predictors())
  expect_equal(names(tr_new), pred_names)
  te_new <- bake(rec, cell_te, all_predictors())
  expect_equal(names(te_new), pred_names)
})

## -----------------------------------------------------------------------------

test_that("tidy method", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) |>
    step_pls(all_predictors(), outcome = HHV, num_comp = 3, id = "dork")

  tidy_pre <- tidy(rec, number = 1)
  exp_pre <- tibble::tribble(
    ~terms,
    ~value,
    ~component,
    ~id,
    "all_predictors()",
    NA_real_,
    NA_character_,
    "dork"
  )
  expect_equal(tidy_pre, exp_pre)

  rec <- prep(rec)
  tidy_post <- tidy(rec, number = 1)
  exp_post <-
    tibble::tribble(
      ~terms,
      ~value,
      ~component,
      ~id,
      "carbon",
      0.82813459059393,
      "PLS1",
      "dork",
      "carbon",
      0.718469477422311,
      "PLS2",
      "dork",
      "carbon",
      0.476111929729498,
      "PLS3",
      "dork",
      "hydrogen",
      -0.206963356355556,
      "PLS1",
      "dork",
      "hydrogen",
      0.642998926998282,
      "PLS2",
      "dork",
      "hydrogen",
      0.262836631090453,
      "PLS3",
      "dork",
      "oxygen",
      -0.49241242430895,
      "PLS1",
      "dork",
      "oxygen",
      0.299176769170812,
      "PLS2",
      "dork",
      "oxygen",
      0.418081563632953,
      "PLS3",
      "dork",
      "nitrogen",
      -0.122633995804743,
      "PLS1",
      "dork",
      "nitrogen",
      -0.172719084680244,
      "PLS2",
      "dork",
      "nitrogen",
      0.642403301090588,
      "PLS3",
      "dork",
      "sulfur",
      0.11768677260853,
      "PLS1",
      "dork",
      "sulfur",
      -0.217341766567037,
      "PLS2",
      "dork",
      "sulfur",
      0.521114256955661,
      "PLS3",
      "dork"
    )
  expect_equal(tidy_post, exp_post, tolerance = 0.01)
})

test_that("check_name() is used", {
  skip_if_not_installed("mixOmics")
  dat <- mtcars
  dat$PLS1 <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_pls(mpg, disp, vs, outcome = am)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

## -----------------------------------------------------------------------------

test_that("Deprecation warning", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_pls(outcome = mpg, preserve = TRUE)
  )
})

test_that("tunable", {
  rec <-
    recipe(Species ~ ., data = iris) |>
    step_pls(all_predictors(), outcome = Species)
  rec_param <- tunable.step_pls(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp", "predictor_prop"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("Do nothing for num_comps = 0 and keep_original_cols = FALSE (#1152)", {
  rec <- recipe(carb ~ ., data = mtcars) |>
    step_pls(
      all_predictors(),
      outcome = carb,
      num_comp = 0,
      keep_original_cols = FALSE
    ) |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_identical(res, tibble::as_tibble(mtcars))
})

test_that("rethrows error correctly from implementation", {
  skip_if_not_installed("mixOmics")
  local_mocked_bindings(
    .package = "mixOmics",
    pls = function(...) {
      cli::cli_abort("mocked error")
    }
  )
  expect_snapshot(
    error = TRUE,
    tmp <- recipe(~., data = mtcars) |>
      step_pls(all_predictors(), outcome = mpg) |>
      prep()
  )
})

test_that("error on no outcome", {
  skip_if_not_installed("mixOmics")
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_pls(all_predictors()) |>
      prep()
  )
})

test_that("check_options() is used", {
  skip_if_not_installed("mixOmics")
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_pls(disp, outcome = mpg, options = TRUE) |>
      prep()
  )
})

test_that("recipes_argument_select() is used", {
  skip_if_not_installed("mixOmics")

  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_pls(disp, outcome = NULL) |>
      prep()
  )
})

test_that("addition of recipes_argument_select() is backwards compatible", {
  skip_if_not_installed("mixOmics")

  rec <- recipe(Species ~ ., data = iris) |>
    step_pls(all_predictors(), outcome = Species) |>
    prep()

  exp <- bake(rec, iris)

  rec$steps[[1]]$outcome <- "Species"

  expect_identical(
    bake(rec, iris),
    exp
  )

  rec_old <- recipe(Species ~ ., data = iris) |>
    step_pls(all_predictors(), outcome = "Species") |>
    prep()

  expect_identical(
    bake(rec_old, iris),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) |>
    step_pls(carbon, outcome = HHV, num_comp = 3) |>
    update_role(carbon, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec <- prep(rec)

  expect_snapshot(error = TRUE, bake(rec, new_data = biom_tr[, c(-1)]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pls(rec, outcome = mpg)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_pls(rec1, outcome = mpg)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pls(rec, outcome = mpg)

  expect <- tibble(
    terms = character(),
    value = double(),
    component = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("mixOmics")
  new_names <- c("vs", "PLS1")

  rec <- recipe(vs ~ mpg, mtcars) |>
    step_pls(all_predictors(), outcome = vs, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(vs ~ mpg, mtcars) |>
    step_pls(all_predictors(), outcome = vs, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("mpg", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(vs ~ mpg, mtcars) |>
    step_pls(all_predictors(), outcome = vs)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = mtcars)
  )
})

test_that("printing", {
  skip_if_not_installed("mixOmics")
  rec <- recipe(HHV ~ ., data = biom_tr) |>
    step_pls(all_predictors(), outcome = HHV, num_comp = 3)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_pls(
      all_predictors(),
      outcome = mpg,
      num_comp = hardhat::tune(),
      predictor_prop = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("bad args", {
  skip_if_not_installed("mixOmics")

  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_pls(-mpg, outcome = mpg, num_comp = -1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_pls(-mpg, outcome = mpg, prefix = 1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(mpg ~ ., data = mtcars) |>
      step_pls(-mpg, outcome = mpg, predictor_prop = -1) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  skip_if_not_installed("mixOmics")

  data <- mtcars
  rec <- recipe(~., data) |>
    step_pls(all_predictors(), outcome = mpg) |>
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
