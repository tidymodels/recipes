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

test_that('dummy variables with factor inputs', {
  dummy <- recipe(~ ., data = languages) %>%
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

test_that('dummy variables with non-factor inputs', {
  dummy <- recipe(~ ., data = mtcars) %>%
    step_dummy_multi_choice(all_predictors())

  expect_error(
    prep(dummy)
  )
})

test_that('printing', {
  rec <- recipe(~ ., data = languages) %>%
    step_dummy_multi_choice(all_predictors())
  expect_output(print(rec))
  expect_output(prep(rec, training = languages, verbose = TRUE))
})

test_that('no columns selected', {
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

  exp_tidy <- tibble(terms = rlang::na_chr, columns = rlang::na_chr,
                     id = rec$steps[[2]]$id)
  expect_equal(exp_tidy, tidy(rec, number = 2))
})

test_that('one columns selected', {
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
