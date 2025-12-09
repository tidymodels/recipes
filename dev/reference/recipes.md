# recipes: A package for computing and preprocessing design matrices.

The `recipes` package can be used to create design matrices for modeling
and to conduct preprocessing of variables. It is meant to be a more
extensive framework that R's formula method. Some differences between
simple formula methods and recipes are that

1.  Variables can have arbitrary roles in the analysis beyond predictors
    and outcomes.

2.  A recipe consists of one or more steps that define actions on the
    variables.

3.  Recipes can be defined sequentially using pipes as well as being
    modifiable and extensible.

## Basic Functions

The three main functions are
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md),
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).

[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
defines the operations on the data and the associated roles. Once the
preprocessing steps are defined, any parameters are estimated using
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md). Once
the data are ready for transformation, the
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
function applies the operations.

## Step Functions

These functions are used to add new actions to the recipe and have the
naming convention `"step_action"`. For example,
[`step_center()`](https://recipes.tidymodels.org/dev/reference/step_center.md)
centers the data to have a zero mean and
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
is used to create dummy variables.

## See also

Useful links:

- <https://github.com/tidymodels/recipes>

- <https://recipes.tidymodels.org/>

- Report bugs at <https://github.com/tidymodels/recipes/issues>

## Author

**Maintainer**: Max Kuhn <max@posit.co>

Authors:

- Hadley Wickham <hadley@posit.co>

- Emil Hvitfeldt <emil.hvitfeldt@posit.co>

Other contributors:

- Posit Software, PBC (03wc8by49) \[copyright holder, funder\]
