library(recipes)
library(testthat)

context("Skipping steps")

test_that('simple skip', {
  rec_1 <- recipe(Sepal.Length ~ ., data = iris) %>%
    step_log(Sepal.Length, skip = TRUE) %>%
    step_dummy(Species) %>%
    step_center(all_predictors())

  prepped_1 <- prep(rec_1, training = iris)

  juiced_1 <- juice(prepped_1)
  baked_1  <- bake(prepped_1, new_data = iris)

  expect_equal(baked_1$Sepal.Length, iris$Sepal.Length)
  expect_equal(juiced_1$Sepal.Length, log(iris$Sepal.Length))

  expect_warning(
    prepped_2 <- prep(rec_1, training = iris, retain = FALSE)
  )

  baked_2  <- bake(prepped_2, new_data = iris[, -1])
  baked_3  <- bake(prepped_2, new_data = iris)
  expect_false(
    isTRUE(
      all.equal(juiced_1$Sepal.Width, baked_3$Sepal.Length)
    )
  )
  expect_equal(log(baked_1$Sepal.Length), juiced_1$Sepal.Length)
  expect_equal(setdiff(names(baked_1), names(baked_2)), "Sepal.Length")

  expect_warning(prep(rec_1, training = iris, retain = FALSE))
})


test_that('check existing steps for `skip` arg', {
  step_check <- ls("package:recipes", pattern = "(^step_)|(^check_)")
  # These ones are not operations
  step_check <- step_check[step_check != "check_type"]
  step_check <- step_check[step_check != "check_nominal_type"]
  step_check <- step_check[step_check != "check_name"]
  step_check <- step_check[step_check != "step_type"]
  has_skip_arg <- function(x) {
    x_code <- getFromNamespace(x, "recipes")
    x_args <- names(formals(x_code))
    "skip" %in% x_args
  }
  has_skip <- vapply(step_check, has_skip_arg, logical(1))
  if(any(!has_skip))
    print(names(has_skip)[!has_skip])

  for(i in names(has_skip))
    expect_true(has_skip[i])
})


test_that('skips for steps that remove columns (#239)', {
  simple_ex <-
    recipe(Species ~ ., data = iris) %>%
    step_interact(terms = ~ Sepal.Length:Sepal.Width) %>%
    step_rm(Sepal.Length, skip = TRUE)

  prep_simple <- prep(simple_ex, iris)
  simple_juiced <- juice(prep_simple)
  simple_baked <- bake(prep_simple, new_data = iris)
  expect_equal(
    sort(names(simple_juiced)),
    c('Petal.Length', 'Petal.Width', 'Sepal.Length_x_Sepal.Width',
      'Sepal.Width', 'Species')
    )
  expect_equal(
    sort(names(simple_baked)),
    c('Petal.Length', 'Petal.Width', 'Sepal.Length',
      'Sepal.Length_x_Sepal.Width', 'Sepal.Width', 'Species')
  )

  complex_ex <-
    recipe(Species ~ ., data = iris) %>%
    step_interact(terms = ~ Sepal.Length:Sepal.Width) %>%
    step_rm(Sepal.Length) %>%
    step_pca(contains("Sepal")) %>%
    step_rm(PC1, skip = TRUE) %>%
    prep()

  complex_juiced <- juice(complex_ex)
  complex_baked <- bake(complex_ex, new_data = iris)

  expect_equal(
    sort(names(complex_juiced)),
    c('PC2', 'Petal.Length', 'Petal.Width', 'Species')
  )
  expect_equal(
    sort(names(complex_baked)),
    c('PC1', 'PC2', 'Petal.Length', 'Petal.Width', 'Species')
  )

  iris_dups <-
    iris %>%
    mutate(
      dup_1 = Sepal.Width,
      dup_2 = Sepal.Width
    )

  corr_example <-
    recipe(Species ~ ., data = iris_dups) %>%
    step_corr(all_predictors(), skip = TRUE) %>%
    prep()

  corr_juiced <- juice(corr_example)
  corr_baked <- bake(corr_example, new_data = iris_dups)

  expect_equal(
    names(corr_juiced),
    c('Sepal.Length', 'Petal.Width', 'dup_2', 'Species')
  )

  expect_equal(
    sort(names(corr_baked)),
    sort(names(iris_dups))
  )
})

