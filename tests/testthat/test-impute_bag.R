library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

biomass$fac <- factor(
  sample(letters[1:2], size = nrow(biomass), replace = TRUE)
)

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + fac,
  data = biomass
)

test_that("imputation models", {
  imputed <- rec |>
    step_impute_bag(
      carbon,
      fac,
      impute_with = c(hydrogen, oxygen),
      seed_val = 12,
      trees = 5
    )

  imputed_trained <- prep(imputed, training = biomass, verbose = FALSE)

  expect_equal(length(imputed_trained$steps[[1]]$models[["carbon"]]$mtrees), 5)

  ## make sure we get the same trees given the same random samples
  carb_samps <- lapply(
    imputed_trained$steps[[1]]$models[["carbon"]]$mtrees,
    function(x) x$bindx
  )
  for (i in seq_along(carb_samps)) {
    carb_data <- biomass[carb_samps[[i]], c("carbon", "hydrogen", "oxygen")]
    carb_mod <- rpart::rpart(
      carbon ~ .,
      data = carb_data,
      control = rpart::rpart.control(xval = 0)
    )
    expect_equal(
      carb_mod$splits,
      imputed_trained$steps[[1]]$models[["carbon"]]$mtrees[[i]]$btree$splits
    )
  }

  fac_samps <- lapply(
    imputed_trained$steps[[1]]$models[[1]]$mtrees,
    function(x) x$bindx
  )

  fac_ctrl <- imputed_trained$steps[[1]]$models[["fac"]]$mtrees[[
    1
  ]]$btree$control

  ## make sure we get the same trees given the same random samples
  for (i in seq_along(fac_samps)) {
    fac_data <- biomass[fac_samps[[i]], c("fac", "hydrogen", "oxygen")]
    fac_mod <- rpart::rpart(fac ~ ., data = fac_data, control = fac_ctrl)
    expect_equal(
      fac_mod$splits,
      imputed_trained$steps[[1]]$models[["fac"]]$mtrees[[i]]$btree$splits
    )
  }

  imp_tibble_un <-
    tibble(
      terms = c("carbon", "fac"),
      model = rep(list(NULL), 2),
      id = imputed_trained$steps[[1]]$id
    )
  imp_tibble_tr <-
    tibble(
      terms = c("carbon", "fac"),
      model = unname(imputed_trained$steps[[1]]$models),
      id = imputed_trained$steps[[1]]$id
    )

  expect_equal(as.data.frame(tidy(imputed, 1)), as.data.frame(imp_tibble_un))
  expect_equal(tidy(imputed_trained, 1)$terms, imp_tibble_tr$terms)
  expect_equal(tidy(imputed_trained, 1)$model, imp_tibble_tr$model)
})

test_that("All NA values", {
  imputed <- rec |>
    step_impute_bag(
      carbon,
      fac,
      impute_with = c(hydrogen, oxygen),
      seed_val = 12,
      trees = 5
    ) |>
    prep(biomass)

  imputed_te <- bake(imputed, biomass |> mutate(carbon = NA))
  expect_equal(sum(is.na(imputed_te$carbon)), 0)
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_impute_bag(
      all_predictors(),
      impute_with = all_predictors()
    )
  rec_param <- tunable.step_impute_bag(rec$steps[[1]])
  expect_equal(rec_param$name, c("trees"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("non-factor imputation", {
  data(scat, package = "modeldata")
  scat$Location <- as.character(scat$Location)
  scat$Location[1] <- NA
  rec <-
    recipe(Species ~ ., data = scat, strings_as_factors = FALSE) |>
    step_impute_bag(Location, impute_with = all_predictors()) |>
    prep()
  expect_true(is.character(bake(rec, NULL, Location)[[1]]))
})

test_that("impute_with errors with nothing selected", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_impute_bag(all_predictors(), impute_with = NULL) |>
      prep()
  )
})

test_that("Warns when impute_with contains all NAs in a row", {
  mtcars[1:3, 1] <- NA_real_
  mtcars[10, 3] <- NA_real_

  mtcars[c(2, 3, 10), 9:10] <- NA_real_

  expect_snapshot(
    tmp <- recipe(~., data = mtcars) |>
      step_impute_bag(mpg, disp, vs, impute_with = c(am, gear)) |>
      prep()
  )
})

test_that("Better error message for nzv fit error (#209)", {
  d <- data.frame(let = c(rep("a", 99), rep("b", 1)), num = seq_len(100))

  expect_snapshot(
    error = TRUE,
    recipe(~., d) |>
      step_impute_bag(let) |>
      prep()
  )
})

test_that("check_options() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(~mpg, data = mtcars) |>
      step_impute_bag(mpg, options = TRUE) |>
      prep()
  )
})

test_that("recipes_argument_select() is used", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_impute_bag(disp, impute_with = NULL) |>
      prep()
  )
})

test_that("addition of recipes_argument_select() is backwards compatible", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_impute_bag(disp) |>
    prep()

  exp <- bake(rec, mtcars)

  rec$steps[[1]]$impute_with <- rlang::new_quosures(
    list(
      rlang::new_quosure(
        quote(all_predictors())
      )
    )
  )

  expect_identical(
    bake(rec, mtcars),
    exp
  )

  rec_old <- recipe(mpg ~ ., data = mtcars) |>
    step_impute_bag(disp, impute_with = imp_vars(all_predictors())) |>
    prep()

  expect_identical(
    bake(rec_old, mtcars),
    exp
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  imputed <- rec |>
    step_impute_bag(
      carbon,
      fac,
      impute_with = c(hydrogen, oxygen),
      seed_val = 12,
      trees = 5
    ) |>
    update_role(carbon, fac, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  imputed_trained <- prep(imputed, training = biomass, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(imputed_trained, new_data = biomass[, c(-3, -9)])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_bag(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_impute_bag(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_bag(rec)

  expect <- tibble(terms = character(), model = list(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(
    HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + fac,
    data = biomass
  ) |>
    step_impute_bag(carbon, impute_with = hydrogen)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_impute_bag(
      all_predictors(),
      trees = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_impute_bag(
        all_predictors(),
        trees = -1
      ) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_impute_bag(
        all_predictors(),
        seed_val = 1:4
      ) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_impute_bag(disp, mpg, impute_with = all_predictors()) |>
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
