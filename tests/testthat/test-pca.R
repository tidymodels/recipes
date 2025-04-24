library(testthat)
library(recipes)
skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

# Note: some tests convert to data frame prior to testing
# https://github.com/tidyverse/dplyr/issues/2751

test_that("correct PCA values", {
  pca_extract <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_pca(
      carbon,
      hydrogen,
      oxygen,
      nitrogen,
      sulfur,
      options = list(retx = TRUE),
      id = ""
    )

  pca_extract_trained <- prep(
    pca_extract,
    training = biomass_tr,
    verbose = FALSE
  )

  pca_pred <- bake(pca_extract_trained, new_data = biomass_te, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- prcomp(
    biomass_tr[, 3:7],
    center = TRUE,
    scale. = TRUE,
    retx = TRUE
  )
  pca_pred_exp <- predict(pca_exp, biomass_te[, 3:7])[,
    1:pca_extract$steps[[3]]$num_comp
  ]

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)

  tidy_exp_un <- tibble(
    terms = c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur"),
    value = rep(NA_real_, 5),
    component = rep(NA_character_, 5),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(pca_extract, number = 3))

  pca_obj <- prcomp(
    x = biomass_tr[, c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur")],
    scale. = TRUE
  )
  variances <- pca_obj$sdev^2
  pca_obj <- pca_obj$rotation
  pca_obj <- as.data.frame(pca_obj)
  pca_obj <- utils::stack(pca_obj)

  tidy_exp_tr <- tibble(
    terms = rep(tidy_exp_un$terms, pca_extract_trained$steps[[3]]$num_comp),
    value = pca_obj$values,
    component = as.character(pca_obj$ind),
    id = ""
  )
  expect_equal(
    as.data.frame(tidy_exp_tr),
    as.data.frame(tidy(pca_extract_trained, number = 3))
  )

  var_obj <- tidy(pca_extract_trained, number = 3, type = "variance")
  expect_equal(
    var_obj$value[var_obj$terms == "variance"],
    variances
  )
  expect_equal(
    var_obj$value[var_obj$terms == "cumulative variance"],
    cumsum(variances)
  )
  expect_equal(
    var_obj$value[var_obj$terms == "percent variance"],
    variances / sum(variances) * 100
  )
  expect_equal(
    var_obj$value[var_obj$terms == "cumulative percent variance"],
    cumsum(variances) / sum(variances) * 100
  )
  expect_snapshot(
    error = TRUE,
    tidy(pca_extract_trained, number = 3, type = "variances")
  )
})

test_that("correct PCA values with threshold", {
  pca_extract <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, threshold = .5)

  pca_extract_trained <- prep(
    pca_extract,
    training = biomass_tr,
    verbose = FALSE
  )
  pca_exp <- prcomp(
    biomass_tr[, 3:7],
    center = TRUE,
    scale. = TRUE,
    retx = TRUE
  )
  # cumsum(pca_exp$sdev^2)/sum(pca_exp$sdev^2)

  expect_equal(pca_extract_trained$steps[[3]]$num_comp, 2)
})

test_that("Reduced rotation size", {
  pca_extract <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, num_comp = 3)

  pca_extract_trained <- prep(
    pca_extract,
    training = biomass_tr,
    verbose = FALSE
  )

  pca_pred <- bake(pca_extract_trained, new_data = biomass_te, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- prcomp(
    biomass_tr[, 3:7],
    center = TRUE,
    scale. = TRUE,
    retx = TRUE
  )
  pca_pred_exp <- predict(pca_exp, biomass_te[, 3:7])[, 1:3]
  rownames(pca_pred_exp) <- NULL

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)
})

test_that("No PCA comps", {
  pca_extract <- rec |>
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur, num_comp = 0)

  pca_extract_trained <- prep(pca_extract, training = biomass_tr)
  expect_equal(
    names(bake(pca_extract_trained, new_data = NULL)),
    names(biomass_tr)[-(1:2)]
  )
  expect_true(all(is.na(pca_extract_trained$steps[[1]]$res$rotation)))
  expect_snapshot(print(pca_extract_trained))
  expect_true(all(is.na(tidy(pca_extract_trained, 1)$value)))
})

test_that("backwards compatible with 0.1.17", {
  pca_extract <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_pca(
      carbon,
      hydrogen,
      oxygen,
      nitrogen,
      sulfur,
      options = list(retx = TRUE),
      id = ""
    ) |>
    prep()

  exp_res <- bake(pca_extract, biomass_tr)

  # Simulate what would have happened in 0.1.17
  pca_extract$steps[[3]]$columns <- NULL

  new_res <- bake(pca_extract, biomass_tr)

  expect_equal(
    exp_res,
    new_res
  )

  expect_snapshot(pca_extract)
})

test_that("check_name() is used", {
  dat <- mtcars
  dat$PC1 <- dat$mpg

  rec <- recipe(~., data = dat) |>
    step_pca(mpg, disp, vs)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_pca(all_predictors())
  rec_param <- tunable.step_pca(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp", "threshold"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("case weights", {
  biomass_tr_cw <- biomass_tr |>
    mutate(nitrogen = frequency_weights(round(nitrogen))) |>
    select(HHV, carbon, hydrogen, oxygen, nitrogen, sulfur)

  pca_extract <- recipe(HHV ~ ., data = biomass_tr_cw) |>
    step_pca(all_numeric_predictors())

  pca_extract_trained <- prep(pca_extract)

  pca_pred <- bake(pca_extract_trained, new_data = biomass_te, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- pca_wts(
    biomass_tr[, c(3, 4, 5, 7)],
    wts = as.numeric(biomass_tr_cw$nitrogen)
  )
  pca_pred_exp <- as.matrix(biomass_te[, c(3, 4, 5, 7)]) %*% pca_exp$rotation

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL
  colnames(pca_pred) <- NULL
  colnames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)

  expect_snapshot(pca_extract_trained)

  # ----------------------------------------------------------------------------

  biomass_tr_cw <- biomass_tr |>
    mutate(nitrogen = importance_weights(nitrogen)) |>
    select(HHV, carbon, hydrogen, oxygen, nitrogen, sulfur)

  pca_extract <- recipe(HHV ~ ., data = biomass_tr_cw) |>
    step_pca(all_numeric_predictors())

  pca_extract_trained <- prep(pca_extract)

  pca_pred <- bake(pca_extract_trained, new_data = biomass_te, all_predictors())
  pca_pred <- as.matrix(pca_pred)

  pca_exp <- prcomp(
    biomass_tr[, c(3, 4, 5, 7)],
    center = FALSE,
    scale. = FALSE,
    retx = FALSE
  )
  pca_pred_exp <- predict(pca_exp, biomass_te[, c(3, 4, 5, 7)])[, 1:4]

  rownames(pca_pred) <- NULL
  rownames(pca_pred_exp) <- NULL
  colnames(pca_pred) <- NULL
  colnames(pca_pred_exp) <- NULL

  expect_equal(pca_pred, pca_pred_exp)

  expect_snapshot(pca_extract_trained)
})

test_that("Do nothing for num_comps = 0 and keep_original_cols = FALSE (#1152)", {
  rec <- recipe(~., data = mtcars) |>
    step_pca(all_predictors(), num_comp = 0, keep_original_cols = FALSE) |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_identical(res, tibble::as_tibble(mtcars))
})

test_that("check_options() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(~mpg, data = mtcars) |>
      step_pca(mpg, options = TRUE) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  pca_extract <- rec |>
    step_pca(
      carbon,
      hydrogen,
      oxygen,
      nitrogen,
      sulfur,
      options = list(retx = TRUE),
      id = ""
    ) |>
    update_role(
      carbon,
      hydrogen,
      oxygen,
      nitrogen,
      sulfur,
      new_role = "potato"
    ) |>
    update_role_requirements(role = "potato", bake = FALSE)

  pca_extract_trained <- prep(
    pca_extract,
    training = biomass_tr,
    verbose = FALSE
  )

  expect_snapshot(
    error = TRUE,
    bake(pca_extract_trained, new_data = biomass_te[, c(-3)])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pca(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_pca(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pca(rec)

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
  new_names <- c("PC1")

  rec <- recipe(~mpg, mtcars) |>
    step_pca(all_predictors(), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~mpg, mtcars) |>
    step_pca(all_predictors(), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("mpg", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~mpg, mtcars) |>
    step_pca(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = mtcars)
  )
})

test_that("printing", {
  rec <- recipe(
    HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
    data = biomass_tr
  ) |>
    step_pca(carbon, hydrogen, oxygen, nitrogen, sulfur)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_pca(
      all_predictors(),
      num_comp = hardhat::tune(),
      threshold = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_pca(all_numeric_predictors(), num_comp = -1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_pca(all_numeric_predictors(), prefix = 1) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_pca(all_numeric_predictors(), threshold = -1) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_pca(all_predictors()) |>
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
