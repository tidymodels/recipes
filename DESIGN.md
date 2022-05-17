# Recipes design document

This document details standards that should be upheld within recipes. This document should act as a central focus point and reference for code patterns and consistency.

# Step functions

## Steps should allow empty selections

Support for empty selections was added in https://github.com/tidymodels/recipes/pull/813. Inspiration for how to handle this can be found in that pull request. The standard tests can be found below

``` r
test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_example(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_example(rec)

  # Make sure this matches the specific step
  expect <- tibble(terms = character(), min = double(), max = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_example(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

```

## Steps that take factor variables should respect levels

Care should be taken when using factor/character variables. Use `levels()` whenever possible.

## Type checking

All steps should check typing whenever possible. `check_type()` can be used in most cases.

## Checking for name collision

All steps that create new variables should use `check_name()` to avoid name collisions.

# Step arguments

## order of arguments

The order of arguments in steps should be in the following order

- recipe
- ...
- role
- trained
- step specific arguments
- options
- res
- columns
- prefix
- keep_original_cols
- skip
- id

## Use of `keep_original_cols` 

`keep_original_cols` are added to steps that creates new variable based on existing variables, such as `step_dummy()` and `step_pca()`.

## how prefix should be used

the `prefix` argument is used with steps that generate additional columns. The general use case should be `{prefix}{number}` for steps with numbers, and `{prefix}_{name}` otherwise.

# Tidy methods

## All steps should have a tidy method

The `tidy()` method of a step is useful to extract trained information from a step. The following code can serve as a template. The result of an unprepped step should match the trained result, using appropriate `NA` values for values that are calculated doing `prep()`.

``` r
tidy.step_example <- function(x, ...) {
  if (is_trained(x)) {
    res <- code_to_create_tibble(x)
  }
  else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        # This should reflext what happens if the recipe is trained
        value = rlang::na_dbl
      )
  }
  res$id <- x$id
  res
}
```

## Columns returned by `tidy()` for a step should not change when the recipe is prepped

The standard test for this rule is seen below. Data might have to be updated to match the step.

``` r
test_that("prep() doesn't change tidy() columns", {
  rec_spec <- recipe(~ ., data = mtcars) %>%
    step_example(all_predictors())
  
  expect_equal(
    rec_spec %>% tidy(1) %>% colnames(),
    rec_spec %>% prep() %>% tidy(1) %>% colnames()
  )
})
```

## Columns returned by `tidy()` should not be named vectors

Each of the columns that are produced by calling `tidy()` on a step should be unnamed. Use of `unname()` is recommended solution.

# Tests

## All steps should snapshot printing

The standard test for this rule is seen below. Data might have to be updated to match the step.

``` r
test_that("printing", {
  rec_spec <- recipe(~., data = mtcars) %>%
    step_example(all_predictors())
  expect_snapshot(rec_spec)
  expect_snapshot(prep(rec_spec))
})
```

# Deprecation

## Deprecation of argument

An argument is deprecated using the typical [deprecation process](https://lifecycle.r-lib.org/articles/stages.html). Beyond making the internals of the steps work with the new argument, there are a couple of additional changes that need to occur. First, the argument default is set to `deprecated()` in `step_example()` definition.

Inside `step_example()`, add the following warning

``` r
if (lifecycle::is_present(old_argument)) {
  lifecycle::deprecate_soft(
    "NEXT CRAN VERSION IN FORMAT X.X.X",
    "step_example(old_argument = )",
    "step_example(new_argument = )"
  )
  new_argument <- old_argument
}
```

Lastly, make sure that the argument is passed correctly to `add_step()`

``` r
add_step(
  recipe,
  step_example_new(
    terms = ellipse_check(...),
    role = role,
    trained = trained,
    old_argument = new_argument,
    new_argument = new_argument,
    columns = columns,
    skip = skip,
    id = id
  )
)
```
