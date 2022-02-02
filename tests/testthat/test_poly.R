library(testthat)
library(recipes)
library(modeldata)
data(biomass)


biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

test_that('correct basis functions', {
  with_poly <- rec %>%
    step_poly(carbon, hydrogen, id = "")

  exp_tidy_un <- tibble(terms = c("carbon", "hydrogen"),
                        degree = rep(2L, 2),
                        id = "")
  expect_equal(exp_tidy_un, tidy(with_poly, number = 1))


  with_poly <- prep(with_poly, training = biomass_tr, verbose = FALSE)

  expect_equal(exp_tidy_un, tidy(with_poly, number = 1))

  with_poly_pred_tr <- bake(with_poly, new_data = biomass_tr)
  with_poly_pred_te <- bake(with_poly, new_data = biomass_te)

  carbon_poly_tr_exp <- poly(biomass_tr$carbon, degree = 2)
  hydrogen_poly_tr_exp <- poly(biomass_tr$hydrogen, degree = 2)
  carbon_poly_te_exp <- predict(carbon_poly_tr_exp, biomass_te$carbon)
  hydrogen_poly_te_exp <- predict(hydrogen_poly_tr_exp, biomass_te$hydrogen)

  carbon_poly_tr_res <- as.matrix(with_poly_pred_tr[, grep("carbon", names(with_poly_pred_tr))])
  colnames(carbon_poly_tr_res) <- NULL
  hydrogen_poly_tr_res <- as.matrix(with_poly_pred_tr[, grep("hydrogen", names(with_poly_pred_tr))])
  colnames(hydrogen_poly_tr_res) <- NULL

  carbon_poly_te_res <- as.matrix(with_poly_pred_te[, grep("carbon", names(with_poly_pred_te))])
  colnames(carbon_poly_te_res) <- 1:ncol(carbon_poly_te_res)
  hydrogen_poly_te_res <- as.matrix(with_poly_pred_te[, grep("hydrogen", names(with_poly_pred_te))])
  colnames(hydrogen_poly_te_res) <- 1:ncol(hydrogen_poly_te_res)

  ## remove attributes
  carbon_poly_tr_exp <- matrix(carbon_poly_tr_exp, ncol = 2)
  carbon_poly_te_exp <- matrix(carbon_poly_te_exp, ncol = 2)
  hydrogen_poly_tr_exp <- matrix(hydrogen_poly_tr_exp, ncol = 2)
  hydrogen_poly_te_exp <- matrix(hydrogen_poly_te_exp, ncol = 2)
  dimnames(carbon_poly_tr_res) <- NULL
  dimnames(carbon_poly_te_res) <- NULL
  dimnames(hydrogen_poly_tr_res) <- NULL
  dimnames(hydrogen_poly_te_res) <- NULL

  expect_equal(carbon_poly_tr_res, carbon_poly_tr_exp)
  expect_equal(carbon_poly_te_res, carbon_poly_te_exp)
  expect_equal(hydrogen_poly_tr_res, hydrogen_poly_tr_exp)
  expect_equal(hydrogen_poly_te_res, hydrogen_poly_te_exp)
})


test_that('printing', {
  with_poly <- rec %>%
    step_poly(carbon, hydrogen)
  expect_output(print(with_poly))
  expect_output(prep(with_poly, training = biomass_tr, verbose = TRUE))
})


test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_poly(all_predictors())
  rec_param <- tunable.step_poly(rec$steps[[1]])
  expect_equal(rec_param$name, c("degree"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})



test_that('old option argument', {
  expect_message(
    res <-
      recipe(~ ., data = iris) %>%
      step_poly(Sepal.Width, options = list(degree = 3)) %>%
      prep() %>%
      juice(),
    "The `degree` argument is now a main argument"
  )
  exp_names <- c('Sepal.Length', 'Petal.Length', 'Petal.Width', 'Species',
                 'Sepal.Width_poly_1', 'Sepal.Width_poly_2', 'Sepal.Width_poly_3')
  expect_equal(
    names(res),
    exp_names
  )
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_poly(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_poly(rec)

  expect <- tibble(terms = character(), degree = integer(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_poly(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
