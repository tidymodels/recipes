# Ordering of steps

In the recipes package, there are no constraints on the order in which
steps are added to the recipe; you as a user are free to apply steps in
the order appropriate to your data preprocessing needs. However, the
**order of steps matters** and there are some general suggestions that
you should consider.

## Transforming a variable

- If using a Box-Cox transformation, don’t center the data first or do
  any operations that might make the data non-positive.
- Alternatively, use the Yeo-Johnson transformation so you don’t have to
  worry about this.

## Handling levels in categorical data

The order of steps for handling categorical levels is important, because
each step sets levels for the next step to use as input. These steps
create *factor* output, even if the input is of character type.

- Typically use
  [`step_novel()`](https://recipes.tidymodels.org/dev/reference/step_novel.md)
  before other steps for changing factor levels, so that the new factor
  level can be set as you desire rather than coerced to `NA` by other
  factor handling steps.
- Use steps like
  [`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md)
  and
  [`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md)
  after other steps for changing factor levels.
- If you are creating dummy variables from a categorical variable (see
  below), complete handling of the categorical variable’s levels
  *before*
  [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md).

## Dummy variables

Recipes do not automatically create dummy variables (unlike *most*
formula methods).

- If you want to center, scale, or do any other operations on *all* of
  the predictors, run
  [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  first so that numeric columns are in the data set instead of factors.
- As noted in the [help file for
  `step_interact()`](https://recipes.tidymodels.org/reference/step_interact.html),
  you should make dummy variables *before* creating the interactions.

## Recommended preprocessing outline

While every individual project’s needs are different, here is a
suggested order of *potential* steps that should work for most problems:

1.  Impute
2.  Handle factor levels
3.  Individual transformations for skewness and other issues
4.  Discretize (if needed and if you have no other choice)
5.  Create dummy variables
6.  Create interactions
7.  Normalization steps (center, scale, range, etc)
8.  Multivariate transformation (e.g. PCA, spatial sign, etc)

Again, your mileage may vary for your particular problem.
