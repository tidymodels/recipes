library(recipes)
library(testthat)


test_that('simple skip', {
  rec_1 <- recipe(Sepal.Length ~ ., data = iris) %>%
    step_log(Sepal.Length, skip = TRUE) %>%
    step_dummy(Species) %>%
    step_center(all_predictors())

  prepped_1 <- prep(rec_1, training = iris, retain = TRUE)

  juiced_1 <- juice(prepped_1)
  baked_1  <- bake(prepped_1, newdata = iris)

  expect_equal(baked_1$Sepal.Length, iris$Sepal.Length)
  expect_equal(juiced_1$Sepal.Length, log(iris$Sepal.Length))

  prepped_2 <- prep(rec_1, training = iris, retain = FALSE)

  baked_2  <- bake(prepped_2, newdata = iris[, -1])
  baked_3  <- bake(prepped_2, newdata = iris)
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
  # This one is not an operation
  step_check <- step_check[step_check != "check_type"]
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


