library(testthat)
library(gower)
library(recipes)
library(dplyr)
skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")


rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass
)

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

# induce some missing data at random
set.seed(9039)
carb_missing <- sample(1:nrow(biomass_te), 3)
nitro_missing <- sample(1:nrow(biomass_te), 3)

biomass_te$carbon[carb_missing] <- NA
biomass_te$nitrogen[nitro_missing] <- NA

test_that("imputation values", {
  discr_rec <- rec %>%
    step_discretize(nitrogen, options = list(keep_na = FALSE))
  impute_rec <- discr_rec %>%
    step_impute_knn(carbon,
      nitrogen,
      impute_with = imp_vars(hydrogen, oxygen, nitrogen),
      neighbors = 3,
      id = ""
    )

  imp_exp_un <- tibble(
    terms = c("carbon", "nitrogen"),
    predictors = rep(NA_character_, 2),
    neighbors = rep(3, 2),
    id = ""
  )
  expect_equal(as.data.frame(tidy(impute_rec, number = 2)), as.data.frame(imp_exp_un))
  discr_rec <- prep(discr_rec, training = biomass_tr, verbose = FALSE)
  tr_data <- bake(discr_rec, new_data = biomass_tr)
  te_data <- bake(discr_rec, new_data = biomass_te) %>%
    dplyr::select(hydrogen, oxygen, nitrogen, carbon)

  nn <- gower_topn(te_data[, c("hydrogen", "oxygen", "nitrogen")],
    tr_data[, c("hydrogen", "oxygen", "nitrogen")],
    n = 3
  )$index

  impute_rec <- prep(impute_rec, training = biomass_tr, verbose = FALSE)
  imputed_te <- bake(impute_rec, new_data = biomass_te)

  for (i in carb_missing) {
    nn_tr_ind <- nn[, i]
    nn_tr_data <- tr_data$carbon[nn_tr_ind]
    expect_equal(imputed_te$carbon[i], mean(nn_tr_data))
  }

  for (i in nitro_missing) {
    nn_tr_ind <- nn[, i]
    nn_tr_data <- tr_data$nitrogen[nn_tr_ind]
    expect_equal(
      as.character(imputed_te$nitrogen[i]),
      recipes:::mode_est(nn_tr_data)
    )
  }


  imp_exp_tr <- tidyr::crossing(
    terms = c("carbon", "nitrogen"),
    predictors = c("hydrogen", "oxygen", "nitrogen")
  )
  imp_exp_tr <- imp_exp_tr[imp_exp_tr$terms != imp_exp_tr$predictors, ]
  imp_exp_tr <- as_tibble(imp_exp_tr) %>%
    mutate(
      neighbors = 3,
      id = ""
    )
  expect_identical(
    imp_exp_tr,
    tidy(impute_rec, number = 2) %>% arrange(terms, predictors)
  )
})

test_that("All NA values", {
  imputed <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
    data = biomass_tr
  ) %>%
    step_impute_knn(carbon,
      nitrogen,
      impute_with = imp_vars(hydrogen, oxygen, nitrogen),
      neighbors = 3
    ) %>%
    prep(biomass_tr)

  imputed_te <- bake(imputed, biomass_te %>% mutate(carbon = NA))
  expect_equal(sum(is.na(imputed_te$carbon)), 0)
})


test_that("printing", {
  discr_rec <- rec %>%
    step_impute_knn(carbon,
      nitrogen,
      impute_with = imp_vars(hydrogen, oxygen, nitrogen),
      neighbors = 3,
      id = ""
    )
  expect_snapshot(print(discr_rec))
  expect_snapshot(prep(discr_rec))
})


test_that("options", {
  rec_1 <- rec %>%
    step_impute_knn(carbon,
      nitrogen,
      impute_with = imp_vars(hydrogen, oxygen, nitrogen),
      neighbors = 3,
      options = list(),
      id = ""
    )
  expect_equal(rec_1$steps[[1]]$options, list(nthread = 1, eps = 1e-08))

  rec_2 <- rec %>%
    step_impute_knn(carbon,
      nitrogen,
      impute_with = imp_vars(hydrogen, oxygen, nitrogen),
      neighbors = 3,
      options = list(nthread = 10),
      id = ""
    )
  expect_equal(rec_2$steps[[1]]$options, list(nthread = 10, eps = 1e-08))

  rec_3 <- rec %>%
    step_impute_knn(carbon,
      nitrogen,
      impute_with = imp_vars(hydrogen, oxygen, nitrogen),
      neighbors = 3,
      options = list(eps = 10),
      id = ""
    )
  expect_equal(rec_3$steps[[1]]$options, list(eps = 10, nthread = 1))

  dat_1 <-
    tibble::tribble(
      ~x, ~y,
      1e-20, -0.135,
      0.371, 1.775,
      -0.399, 0.068,
      -0.086, -0.511,
      -1.094, -0.342,
      -1.096, -0.812,
      0.012, 0.937,
      -0.89, -0.579,
      -1.128, 0.14,
      -1.616, 0.619
    )
  dat_1$x[1] <- 10^(-20)

  dat_2 <-
    tibble::tribble(
      ~x, ~y,
      -0.573, 0.922
    )

  ref_nn <- gower_topn(x = dat_2, y = dat_1, n = 2)$index
  expect_snapshot(new_nn <- gower_topn(x = dat_2, y = dat_1, n = 2, eps = 2)$index)
  expect_false(isTRUE(all.equal(ref_nn, new_nn)))
})



test_that("tunable", {
  rec <-
    recipe(~., data = iris) %>%
    step_impute_knn(all_predictors())
  rec_param <- tunable.step_impute_knn(rec$steps[[1]])
  expect_equal(rec_param$name, c("neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_impute_knn(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_knn(rec)

  expect <- tibble(
    terms = character(),
    predictors = character(),
    neighbors = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_impute_knn(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  imputed <-
    recipe(HHV ~ carbon + hydrogen + oxygen, data = biomass) %>%
    step_impute_knn(carbon, impute_with = imp_vars(hydrogen, oxygen)) %>%
    update_role(hydrogen, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  imputed_trained <- prep(imputed, training = biomass, verbose = FALSE)

  expect_error(bake(imputed_trained, new_data = biomass[, c(-4)]),
               class = "new_data_missing_column")
})
