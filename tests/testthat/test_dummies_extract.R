library(testthat)
library(recipes)

data(tate_text, package = "modeldata")

color_examples <- tibble(
  colors = c("['red', 'blue']",
             "['red', 'blue', 'white']",
             "['blue', 'blue', 'blue']")
)

color_result <- tribble(
  ~colors_blue, ~colors_red, ~colors_white, ~colors_other,
  1,            1,           0,             0,
  1,            1,           1,             0,
  3,            0,           0,             0
)

mini_tate <- tate_text[c(101, 102, 105, 108), ]

mini_tate_result <- tibble(
  medium_Charcoal = c(0, 0, 0, 1),
  medium_Etching =  c(1, 1, 1, 0),
  medium_aquatint = c(1, 0, 0, 0),
  medium_gouache =  c(0, 0, 0, 1),
  medium_paper =    c(1, 1, 1, 1),
  medium_other =    c(0, 0, 0, 0)
)

test_that('dummy variables', {
  # Using `sep` argument
  dummy <- recipe(~ medium, data = mini_tate) %>%
    step_dummy_extract(medium, sep = "( and )|( on )", id = "")

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = mini_tate)

  expect_identical(dummy_pred, mini_tate_result)
  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "medium",
      columns = c("paper","Etching", "Charcoal", "aquatint", "gouache"),
      id = ""
    )
  )

  # Using `pattern` argument
  dummy <- recipe(~ colors, data = color_examples) %>%
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", id = "")

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(dummy_pred, color_result)
  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:3],
      id = ""
    )
  )
})

test_that('other argument', {
  # Using `sep` argument
  dummy <- recipe(~ medium, data = mini_tate) %>%
    step_dummy_extract(medium, sep = "( and )|( on )", id = "", other = "cake")

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = mini_tate)

  expect_identical(names(dummy_pred)[length(dummy_pred)], "medium_cake")
})

test_that('error when neither sep or pattern is specified', {
  expect_snapshot(error = TRUE,
    recipe(~ medium, data = tate_text) %>%
      step_dummy_extract(medium) %>%
      prep()
  )
})

test_that('dummy variables with threshold', {
  # threshold = 0.5
  dummy <- recipe(~ colors, data = color_examples) %>%
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", id = "",
                     threshold = 0.5)

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result %>%
      mutate(colors_other = colors_other + colors_white) %>%
      select(-colors_white)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:2],
      id = ""
    )
  )

  # threshold = 0.8
  dummy <- recipe(~ colors, data = color_examples) %>%
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", id = "",
                     threshold = 0.8)

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result %>%
      mutate(colors_other = colors_other + colors_white + colors_red) %>%
      select(-colors_white, -colors_red)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1],
      id = ""
    )
  )
})

test_that('dummy variables with integer threshold', {
  # threshold = 1
  dummy <- recipe(~ colors, data = color_examples) %>%
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", id = "",
                       threshold = 1)

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:3],
      id = ""
    )
  )

  # threshold = 2
  dummy <- recipe(~ colors, data = color_examples) %>%
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", id = "",
                       threshold = 2)

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result %>%
      mutate(colors_other = colors_other + colors_white) %>%
      select(-colors_white)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1:2],
      id = ""
    )
  )

  # threshold = 3
  dummy <- recipe(~ colors, data = color_examples) %>%
    step_dummy_extract(colors, pattern = "(?<=')[^',]+(?=')", id = "",
                       threshold = 3)

  dummy_prepped <- prep(dummy)
  dummy_pred <- bake(dummy_prepped, new_data = color_examples)

  expect_identical(
    dummy_pred,
    color_result %>%
      mutate(colors_other = colors_other + colors_white + colors_red) %>%
      select(-colors_white, -colors_red)
  )

  expect_identical(
    tidy(dummy_prepped, 1),
    tibble(
      terms = "colors",
      columns = gsub("colors_", "", names(color_result))[1],
      id = ""
    )
  )
})

test_that('naming function', {
  expect_equal(
    dummy_extract_names("x", letters[c(1, 2, 3, 3)]),
    c("x_a", "x_b", "x_c", "x_c_2")
  )
  expect_equal(
    dummy_extract_names("x", letters[c(1, 2, 3, 3)], ordinal = TRUE),
    c("x_1", "x_2", "x_3", "x_4")
  )
})

test_that('printing', {
  rec <- recipe(~ medium, data = tate_text) %>%
    step_dummy_extract(all_predictors(), sep = ", ")
  expect_output(print(rec))
  expect_output(prep(rec, training = tate_text, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_dummy_extract(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_extract(rec)

  expect <- tibble(terms = character(), columns = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_dummy_extract(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
