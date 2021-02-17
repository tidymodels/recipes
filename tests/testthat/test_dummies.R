library(testthat)
library(recipes)

context("Dummy variable creation")


library(modeldata)
data(okc)

okc_missing <- okc

okc$diet[is.na(okc$diet)] <- "missing"
okc <- okc[complete.cases(okc), -5]

okc_fac <- okc
okc_fac$diet <- factor(okc_fac$diet)
okc_fac$location <- factor(okc_fac$location)

test_that('dummy variables with factor inputs', {
  rec <- recipe(age ~ location + diet, data = okc_fac)
  dummy <- rec %>% step_dummy(diet, location, id = "")
  dummy_trained <- prep(dummy, training = okc_fac, verbose = FALSE, strings_as_factors = FALSE)
  dummy_pred <- bake(dummy_trained, new_data = okc_fac, all_predictors())

  expect_false(any(colnames(dummy_pred) == "diet"))
  expect_false(any(colnames(dummy_pred) == "location"))

  dummy_pred <- dummy_pred[, order(colnames(dummy_pred))]
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  exp_res <- model.matrix(age ~ location + diet, data = okc_fac)[, -1]
  exp_res <- exp_res[, colnames(exp_res) != "age"]
  colnames(exp_res) <- gsub("^location", "location_", colnames(exp_res))
  colnames(exp_res) <- gsub("^diet", "diet_", colnames(exp_res))
  colnames(exp_res) <- make.names(colnames(exp_res))
  exp_res <- exp_res[, order(colnames(exp_res))]
  exp_res <- as.data.frame(exp_res)
  rownames(exp_res) <- NULL
  expect_equivalent(dummy_pred, exp_res)

  dum_tibble <-
    tibble(terms = c("diet", "location"), columns = rep(rlang::na_chr, 2), id = "")
  dum_tibble_prepped_1 <-
    tibble(
      terms = "diet",
      columns = attributes(dummy_trained$steps[[1]]$levels$diet)$values,
      id = ""
    ) %>% slice(-1)
  dum_tibble_prepped_2 <-
    tibble(
      terms = "location",
      columns = attributes(dummy_trained$steps[[1]]$levels$location)$values,
      id = ""
    ) %>% slice(-1)
  expect_equal(tidy(dummy, 1), dum_tibble)
  expect_equal(
    tidy(dummy_trained, 1),
    bind_rows(dum_tibble_prepped_1, dum_tibble_prepped_2)
  )
})

test_that('dummy variables with non-factor inputs', {
  rec <- recipe(age ~ location + diet, data = okc)
  dummy <- rec %>% step_dummy(diet, location)

  expect_warning(
    expect_error(
      prep(dummy, training = okc, verbose = FALSE, strings_as_factors = FALSE)
    )
  )

  okc_fac_ish <-
    okc_fac %>%
    mutate(diet = as.character(diet))

  expect_warning(
    recipe(age ~ location + height + diet, data = okc_fac_ish) %>%
      step_dummy(diet, location, height) %>%
      prep(training = okc_fac_ish, verbose = FALSE, strings_as_factors = FALSE)
  )

})

test_that('create all dummy variables', {
  rec <- recipe(age ~ location + diet + height, data = okc_fac)
  dummy <- rec %>% step_dummy(diet, location, one_hot = TRUE, id = "")
  dummy_trained <- prep(dummy, training = okc_fac, verbose = FALSE, strings_as_factors = FALSE)
  dummy_pred <- bake(dummy_trained, new_data = okc_fac, all_predictors())
  dummy_pred <- dummy_pred[, order(colnames(dummy_pred))]
  dummy_pred <- as.data.frame(dummy_pred)
  rownames(dummy_pred) <- NULL

  exp_res <- NULL
  for (pred in c("diet", "height", "location")) {
    tmp <- model.matrix(as.formula(paste("~", pred, "+ 0")), data = okc_fac)
    colnames(tmp) <- gsub(paste0("^", pred), paste0(pred, "_"), colnames(tmp))
    exp_res <- bind_cols(exp_res, as_tibble(tmp))
  }
  colnames(exp_res) <- make.names(colnames(exp_res))
  exp_res <- as.data.frame(exp_res)
  rownames(exp_res) <- NULL
  expect_equivalent(dummy_pred, exp_res)

  dum_tibble <-
    tibble(terms = c("diet", "location"), columns = rep(rlang::na_chr, 2), id = "")
  dum_tibble_prepped_1 <-
    tibble(
      terms = "diet",
      columns = attributes(dummy_trained$steps[[1]]$levels$diet)$values,
      id = ""
    )
  dum_tibble_prepped_2 <-
    tibble(
      terms = "location",
      columns = attributes(dummy_trained$steps[[1]]$levels$location)$values,
      id = ""
    )
  expect_equal(
    tidy(dummy_trained, 1),
    bind_rows(dum_tibble_prepped_1, dum_tibble_prepped_2)
  )

})

test_that('tests for issue #91', {
  rec <- recipe(~ diet, data = okc)
  factors <- rec %>% step_dummy(diet)
  factors <- prep(factors, training = okc)
  factors_data_1 <- bake(factors, new_data = okc)
  # Remove one category in diet
  factors_data_2 <- bake(factors, new_data = okc %>% filter(diet != 'halal'))
  expect_equal(names(factors_data_1), names(factors_data_2))

  # now with ordered factor

  okc$ordered_diet <- as.ordered(okc$diet)
  rec <- recipe(~ ordered_diet, data = okc)
  orderedfac <- rec %>% step_dummy(ordered_diet)
  orderedfac <- prep(orderedfac, training = okc)
  ordered_data_1 <- bake(orderedfac, new_data = okc)
  # Remove one category in diet
  ordered_data_2 <- bake(orderedfac, new_data = okc %>% filter(diet != 'halal'))
  expect_equal(names(ordered_data_1), names(ordered_data_2))

})

test_that('tests for NA values in factor', {
  rec <- recipe(~ diet, data = okc_missing)
  factors <- rec %>% step_dummy(diet)
  expect_warning(
    factors <- prep(factors, training = okc_missing)
  )

  factors_data_0 <- juice(factors)
  expect_warning(
    factors_data_1 <- bake(factors, new_data = okc_missing)
  )

  expect_true(
    all(complete.cases(factors_data_0) == complete.cases(okc_missing[, "diet"]))
  )
  expect_true(
    all(complete.cases(factors_data_1) == complete.cases(okc_missing[, "diet"]))
  )
})

test_that('tests for NA values in ordered factor', {
  okc_ordered <- okc_missing
  okc_ordered$diet <- as.ordered(okc_ordered$diet)
  rec <- recipe(~ diet, data = okc_ordered)
  factors <- rec %>% step_dummy(diet)
  expect_warning(
    factors <- prep(factors, training = okc_ordered)
  )

  factors_data_0 <- juice(factors)
  expect_warning(
    factors_data_1 <- bake(factors, new_data = okc_ordered)
  )

  expect_true(
    all(complete.cases(factors_data_0) == complete.cases(okc_ordered[, "diet"]))
  )
  expect_true(
    all(complete.cases(factors_data_1) == complete.cases(okc_ordered[, "diet"]))
  )
})



test_that('new levels', {
  df <- data.frame(
    y = c(1,0,1,1,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,0),
    x1 = c('A','B','B','B','B','A','A','A','B','A','A','B',
           'A','C','C','B','A','B','C','A'),
    stringsAsFactors = FALSE)
  training <- df[1:10,]
  testing <- df[11:20,]
  training$y <- as.factor(training$y)
  training$x1 <- as.factor(training$x1)
  testing$y <- as.factor(testing$y)
  testing$x1 <- as.factor(testing$x1)

  expect_warning(
    recipes:::warn_new_levels(testing$x1, levels(training$x1))
  )
  expect_silent(
    recipes:::warn_new_levels(training$x1, levels(training$x1))
  )

  rec <- recipe(y ~ x1, data = training) %>%
    step_dummy(x1)
  expect_silent(
    rec <- prep(rec, training = training)
  )
  expect_warning(
    bake(rec, new_data = testing)
  )
})

test_that('tests for issue #301', {

  rec <- recipe(~ Species, data = iris)
  dummies <- rec %>% step_dummy(Species)
  dummies <- prep(dummies, training = iris)
  expect_equal(NULL, attr(dummies$steps[[1]]$levels$Species, ".Environment"))

  saved_recipe <- tempfile()
  saveRDS(dummies, file = saved_recipe)
  read_recipe <- readRDS(file = saved_recipe)
  unlink(saved_recipe)
  expect_equal(bake(dummies, new_data = iris), bake(read_recipe, new_data = iris))

  saved_dummies <- dummies
  saved_recipe <- tempfile()
  save(saved_dummies, file = saved_recipe)
  rm(saved_dummies)
  load(file = saved_recipe)
  unlink(saved_recipe)
  expect_equal(bake(dummies, new_data = iris), bake(saved_dummies, new_data = iris))

})



test_that('naming function', {
  expect_equal(dummy_names("x", letters[1:3]), c("x_a", "x_b", "x_c"))
  expect_equal(dummy_names("x", letters[1:3], ordinal = TRUE),
               c("x_1", "x_2", "x_3"))
})

test_that('printing', {
  rec <- recipe(age ~ ., data = okc_fac)
  dummy <- rec %>% step_dummy(diet, location)
  expect_output(print(dummy))
  expect_output(prep(dummy, training = okc_fac, verbose = TRUE))
})


test_that('no columns selected', {
  zdat <- tibble(
    y = c(1, 2, 3),
    x = c("a", "a", "a"),
    z = 3:1
  )

  rec <- recipe(y ~ ., data = zdat) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal()) %>%
    prep(training = zdat)

  expect_null(rec$steps[[2]]$levels)

  expect_equal(names(bake(rec, zdat)), c("z", "y"))

  expect_output(print(rec), regexp = "since no columns were selected")

  exp_tidy <- tibble(terms = rlang::na_chr, columns = rlang::na_chr,
                     id = rec$steps[[2]]$id)
  expect_equal(exp_tidy, tidy(rec, number = 2))
})

test_that('retained columns', {
  rec <- recipe(age ~ location + diet, data = okc_fac)
  dummy <- rec %>% step_dummy(diet, location, preserve = TRUE, id = "")
  dummy_trained <- prep(dummy, training = okc_fac)
  dummy_pred <- bake(dummy_trained, new_data = okc_fac, all_predictors())

  expect_true(any(colnames(dummy_pred) == "diet"))
  expect_true(any(colnames(dummy_pred) == "location"))
})

test_that('keep_original_cols works', {
  rec <- recipe(age ~ diet, data = okc_fac)
  dummy <- rec %>% step_dummy(diet, id = "", keep_original_cols = TRUE)
  dummy_trained <- prep(dummy, training = okc_fac, verbose = FALSE)
  dummy_pred <- bake(dummy_trained, new_data = okc_fac, all_predictors())

  expect_equal(
    colnames(dummy_pred),
    c("diet",
      paste0("diet_", setdiff(gsub(" ", ".", levels(okc_fac$diet)), "anything")))
  )
})

test_that('can prep recipes with no keep_original_cols', {
  rec <- recipe(age ~ diet, data = okc_fac)
  dummy <- rec %>% step_dummy(diet, id = "", keep_original_cols = TRUE)

  dummy$steps[[1]]$keep_original_cols <- NULL

  expect_warning(
    dummy_trained <- prep(dummy, training = okc_fac, verbose = FALSE),
    "'keep_original_cols' was added to"
  )

  expect_error(
    dummy_pred <- bake(dummy_trained, new_data = okc_fac, all_predictors()),
    NA
  )
})
