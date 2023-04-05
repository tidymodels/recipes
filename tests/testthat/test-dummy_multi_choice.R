library(testthat)
library(recipes)

languages <- tribble(
  ~lang_1,    ~lang_2,   ~lang_3,  ~lang_4,
  "English",  "Italian", NA,       NA,
  "Spanish",  NA,        "French", NA,
  "Armenian", "English", "French", NA,
  NA,         NA,        NA,       NA
)

result <- tribble(
  ~Armenian, ~English, ~French, ~Italian, ~Spanish,
  0L,        1L,       0L,      1L,       0L,
  0L,        0L,       1L,      0L,       1L,
  1L,        1L,       1L,      0L,       0L,
  0L,        0L,       0L,      0L,       0L
)

test_that("dummy variables with factor inputs", {
  dummy <- recipe(~., data = languages) %>%
    step_dummy_multi_choice(all_predictors())

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = languages)

  expect_identical(
    unname(unclass(dummy_pred)),
    unname(unclass(result))
  )

  expect_identical(
    names(dummy_pred),
    dummy_names(names(languages)[1], names(result))
  )
})

test_that("dummy variables with non-factor inputs", {
  dummy <- recipe(~., data = mtcars) %>%
    step_dummy_multi_choice(all_predictors())

  expect_snapshot(error = TRUE, prep(dummy))
})

test_that("check_name() is used", {
  dat <- iris
  dat$Species_setosa <- dat$Species

  rec <- recipe(~., data = dat) |>
    step_dummy_multi_choice(Species)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("printing", {
  rec <- recipe(~., data = languages) %>%
    step_dummy_multi_choice(all_predictors())
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable", {
  rec <-
    recipe(~., data = languages) %>%
    step_dummy_multi_choice(all_predictors())
  rec_param <- tunable.step_dummy_multi_choice(rec$steps[[1]])
  expect_equal(rec_param$name, c("threshold"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_dummy_multi_choice(
      all_predictors(),
      threshold = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("no columns selected", {
  zdat <- tibble(
    y = c(1, 2, 3),
    x = c("a", "a", "a"),
    z = 3:1
  )

  rec <- recipe(y ~ ., data = zdat) %>%
    step_zv(all_predictors()) %>%
    step_dummy_multi_choice(all_nominal()) %>%
    prep(training = zdat)

  expect_equal(
    unname(rec$steps[[2]]$input),
    character()
  )

  expect_equal(names(bake(rec, zdat)), c("z", "y"))

  exp_tidy <- tibble(terms = character(), columns = character(), id = character())
  expect_equal(exp_tidy, tidy(rec, number = 2))
})

test_that("one columns selected", {
  zdat <- tibble(
    y = c(1, 2, 3),
    x = c("a", "a", "a"),
    z = 3:1
  )

  rec <- recipe(y ~ ., data = zdat) %>%
    step_dummy_multi_choice(all_nominal()) %>%
    prep(training = zdat)

  expect_equal(
    unname(rec$steps[[1]]$input),
    "x"
  )

  expect_equal(names(bake(rec, zdat)), c("z", "y", "x_a"))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_dummy_multi_choice(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_multi_choice(rec)

  expect <- tibble(terms = character(), columns = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_multi_choice(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("factor levels are preserved", {
  # old data
  tr <- data.frame(x = factor(c("a", "b", "c"), levels = c("a", "b", "c", "d", "e", "f", "g")))

  # new data
  te <- data.frame(x = factor(c("c", "d", "e"), levels = c("a", "b", "c", "d", "e", "f", "g")))
  data1 <- tr %>%
    recipe() %>%
    step_dummy(x, one_hot = T) %>%
    prep() %>%
    bake(new_data = te)

  data2 <- tr %>%
    recipe() %>%
    step_dummy_multi_choice(x, threshold = 0) %>%
    prep() %>%
    bake(new_data = te)

  expect_identical(ncol(data1), ncol(data2))
})
