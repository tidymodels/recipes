library(testthat)
library(recipes)

dat_tr <- data.frame(
  x1 = 1:10,
  x2 = (1:10) + 1,
  x3 = (1:10) + 2,
  x4 = (1:10) + 3,
  x5 = (1:10) + 4,
  y = -(101:110),
  z = factor(rep_len(letters[1:3], 10))
)
dat_te <- data.frame(
  x1 = 21:30,
  x2 = (21:30) + 1,
  x3 = (21:30) + 2,
  x4 = (21:30) + 3,
  x5 = (21:30) + 3,
  y = -(201:210),
  z = factor(rep_len(letters[3:1], 10))
)

rec <- recipe(y ~ ., data = dat_tr)

test_that("non-factor variables with dot", {
  int_rec <- rec %>% step_interact(~ (. - y - z)^3, sep = ":")
  int_rec_trained <-
    prep(int_rec, training = dat_tr, verbose = FALSE)

  te_new <-
    bake(int_rec_trained, new_data = dat_te, all_predictors(), -all_nominal())
  te_new <- te_new[, sort(names(te_new))]
  te_new <- as.matrix(te_new)

  og_terms <- terms(~ (. - y - z)^3, data = dat_te)
  te_og <- model.matrix(og_terms, data = dat_te)[, -1]
  te_og <- te_og[, sort(colnames(te_og))]

  rownames(te_new) <- NULL
  rownames(te_og) <- NULL

  expect_equal(te_og, te_new)
})


test_that("non-factor variables with specific variables", {
  int_rec <- rec %>% step_interact(~ x1:x2 + x3:x4:x5, sep = ":")
  int_rec_trained <-
    prep(int_rec, training = dat_tr, verbose = FALSE)

  te_new <-
    bake(int_rec_trained, new_data = dat_te, all_predictors(), -all_nominal())
  te_new <- te_new[, sort(names(te_new))]
  te_new <- as.matrix(te_new)

  og_terms <- terms(~ x1 + x2 + x3 + x4 + x5 +
    x1:x2 + x3:x4:x5, data = dat_te)
  te_og <- model.matrix(og_terms, data = dat_te)[, -1]
  te_og <- te_og[, sort(colnames(te_og))]

  rownames(te_new) <- NULL
  rownames(te_og) <- NULL

  expect_equal(te_og, te_new)
})


test_that("using selectors", {
  int_rec <- rec %>%
    step_dummy(z) %>%
    step_interact(~ starts_with("z"):x1)
  int_rec_trained <-
    prep(int_rec, training = dat_tr, verbose = FALSE)

  te_new <-
    bake(int_rec_trained, new_data = dat_te, all_predictors(), -all_nominal())
  te_new <- te_new[, sort(names(te_new))]
  te_new <- as.matrix(te_new)
  te_new <- te_new[, c(
    "x1", "x2", "x3", "x4", "x5",
    "z_b", "z_c", "z_b_x_x1", "z_c_x_x1"
  )]

  og_terms <- terms(~ x1 + x2 + x3 + x4 + x5 +
    x1 * z, data = dat_te)
  te_og <- model.matrix(og_terms, data = dat_te)[, -1]
  colnames(te_og) <- gsub(":", "_x_", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("zb", "z_b", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("zc", "z_c", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("x1_x_z_b", "z_b_x_x1", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("x1_x_z_c", "z_c_x_x1", colnames(te_og), fixed = TRUE)

  rownames(te_new) <- NULL
  rownames(te_og) <- NULL

  expect_equal(te_og, te_new)
})


# Tests related to #648
# https://github.com/tidymodels/recipes/issues/648
test_that("using selectors when namespaces with ::", {
  int_rec <- rec %>%
    step_dummy(z) %>%
    step_interact(~ tidyselect::starts_with("z"):x1)
  int_rec_trained <-
    prep(int_rec, training = dat_tr, verbose = FALSE)

  te_new <-
    bake(int_rec_trained, new_data = dat_te, all_predictors(), -all_nominal())
  te_new <- te_new[, sort(names(te_new))]
  te_new <- as.matrix(te_new)
  te_new <- te_new[, c(
    "x1", "x2", "x3", "x4", "x5",
    "z_b", "z_c", "z_b_x_x1", "z_c_x_x1"
  )]

  og_terms <- terms(~ x1 + x2 + x3 + x4 + x5 +
    x1 * z, data = dat_te)
  te_og <- model.matrix(og_terms, data = dat_te)[, -1]
  colnames(te_og) <- gsub(":", "_x_", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("zb", "z_b", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("zc", "z_c", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("x1_x_z_b", "z_b_x_x1", colnames(te_og), fixed = TRUE)
  colnames(te_og) <- gsub("x1_x_z_c", "z_c_x_x1", colnames(te_og), fixed = TRUE)

  rownames(te_new) <- NULL
  rownames(te_og) <- NULL

  expect_equal(te_og, te_new)
})

test_that("using where() works", {
  ex_rec <- rec %>%
    step_dummy(z) %>%
    step_interact(~ where(is.numeric):x1)

  x <- prep(ex_rec, dat_tr)

  expr <- x$steps[[2]]$terms[[2]]
  expect <- expr(`:`(x1 + x2 + x3 + x4 + x5 + y + z_b + z_c, x1))

  expect_equal(
    expr,
    expect
  )
})

test_that("using all_of() works", {
  xvars <- c("x2", "x3")

  ex_rec <- rec %>%
    step_interact(~ all_of(xvars):x1)

  x <- prep(ex_rec, dat_tr)

  expr <- x$steps[[1]]$terms[[2]]
  expect <- expr(`:`(x2 + x3, x1))

  expect_equal(
    expr,
    expect
  )
})

test_that("check_name() is used", {
  dat <- dat_tr
  dat$x1ax2 <- dat$x1

  rec <- recipe(y ~ ., data = dat) %>%
    step_interact(~ (. - y - z)^3, sep = "a")

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# more missing data tests


test_that("finding selectors in formulas", {
  expect_equal(
    recipes:::find_selectors(
      ~ (a + b + starts_with("huh?")):has_role("something")
    ),
    list(quote(starts_with("huh?")), quote(has_role("something")))
  )
  expect_equal(
    recipes:::find_selectors(
      ~ all_predictors() + something
    ),
    list(quote(all_predictors()))
  )
  expect_equal(
    recipes:::find_selectors(
      ~ (matches("wat?"))^2
    ),
    list(quote(matches("wat?")))
  )
  expect_equal(
    recipes:::find_selectors(
      ~a
    ),
    list()
  )
})

test_that("replacing selectors in formulas", {
  expect_equal(
    recipes:::replace_selectors(
      ~ (a + b + starts_with("huh?")):has_role("something"),
      quote(starts_with("huh?")),
      quote((x1 + x2 + x3))
    ),
    expr(~ (a + b + (x1 + x2 + x3)):has_role("something"))
  )
  expect_equal(
    recipes:::replace_selectors(
      ~ (matches("wat?"))^2,
      quote(matches("wat?")),
      quote(a)
    ),
    expr(~ (a)^2)
  )
  expect_equal(
    recipes:::replace_selectors(
      ~ a + all_predictors(),
      quote(all_predictors()),
      quote(a)
    ),
    expr(~ a + a)
  )
})

test_that("missing columns", {
  skip("redundant with check_new_data checks")

  no_fail <-
    rec %>%
    step_rm(x1) %>%
    step_interact(~ x1:x2)
  expect_snapshot(no_fail_rec <- prep(no_fail, dat_tr))
  no_fail_res <- bake(no_fail_rec, new_data = NULL) %>% names()
  expect_true(!any(grepl("_x_", no_fail_res)))

  one_int <-
    rec %>%
    step_rm(x1) %>%
    step_interact(~ x1:x2) %>%
    step_interact(~ x3:x2)
  expect_snapshot(one_int_rec <- prep(one_int, dat_tr))
  one_int_res <- bake(one_int_rec, new_data = NULL) %>% names()
  expect_true(sum(grepl("_x_", one_int_res)) == 1)

  with_selectors <-
    rec %>%
    step_rm(x1) %>%
    step_interact(~ starts_with("x"):starts_with("x")) %>%
    step_interact(~ x3:x2)
  expect_warning(prep(with_selectors, dat_tr), regexp = NA)
})


# currently failing; try to figure out why
# test_that('with factors', {
#   int_rec <- recipe(Sepal.Width ~ ., data = iris) %>%
#     step_interact(~ (. - Sepal.Width)^3, sep = ":")
#   int_rec_trained <- prep(int_rec, iris)
#
#   te_new <- bake(int_rec_trained, new_data = iris, role = "predictor")
#   te_new <- te_new[, sort(names(te_new))]
#   te_new <- as.matrix(te_new)
#
#   og_terms <- terms(Sepal.Width ~ (.)^3, data = iris)
#   te_og <- model.matrix(og_terms, data = iris)[, -1]
#   te_og <- te_og[, sort(colnames(te_og))]
#
#   rownames(te_new) <- NULL
#   rownames(te_og) <- NULL
#
#   all.equal(te_og, te_new)
# })

test_that("works when formula is passed in as an object", {
  rec1 <- recipe(~., data = mtcars) %>%
    step_interact(terms = ~vs:am, id = "") %>%
    prep()

  cars_formula <- ~ vs:am
  rec2 <- recipe(~., data = mtcars) %>%
    step_interact(terms = cars_formula, id = "") %>%
    prep()

  expect_identical(rec1, rec2)

  cars_formula <- ~ vs:am
  rec3 <- recipe(~., data = mtcars) %>%
    step_interact(terms = !!cars_formula, id = "") %>%
    prep()

  expect_identical(rec1, rec3)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  int_rec <- rec %>%
    step_interact(~ starts_with("z"):x1, id = "") %>%
    update_role(starts_with("z"):x1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  suppressWarnings(
    int_rec_trained <-
      prep(int_rec, training = dat_tr, verbose = FALSE)
  )

  expect_error(bake(int_rec_trained, dat_tr[, 4:6]),
               class = "new_data_missing_column")

  expect_snapshot(bake(int_rec_trained, dat_tr[, 4:6]), error = TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_interact(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_center(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_interact(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  new_names <- c("x1_x_x2")

  rec <- recipe(~ x1 + x2, data = dat_tr) %>%
    step_interact(~ x1:x2, keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~ x1 + x2, data = dat_tr) %>%
    step_interact(~ x1:x2, keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("x1", "x2", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  rec <- recipe(~ x1 + x2, data = dat_tr) %>%
    step_interact(~ x1:x2, keep_original_cols = FALSE)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_error(
    bake(rec, new_data = dat_tr),
    NA
  )
})

test_that("printing", {
  rec <- recipe(y ~ ., data = dat_tr) %>%
    step_interact(~ x1:x2)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
