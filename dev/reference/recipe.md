# Create a recipe for preprocessing data

A recipe is a description of the steps to be applied to a data set in
order to prepare it for data analysis.

## Usage

``` r
recipe(x, ...)

# Default S3 method
recipe(x, ...)

# S3 method for class 'data.frame'
recipe(
  x,
  formula = NULL,
  ...,
  vars = NULL,
  roles = NULL,
  strings_as_factors = NULL
)

# S3 method for class 'formula'
recipe(formula, data, ...)

# S3 method for class 'matrix'
recipe(x, ...)
```

## Arguments

- x, data:

  A data frame, tibble, or sparse matrix from the `Matrix` package of
  the *template* data set. See
  [sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
  for more information about use of sparse data. (see below).

- ...:

  Further arguments passed to or from other methods (not currently
  used).

- formula:

  A model formula. No in-line functions should be used here (e.g.
  `log(x)`, `x:y`, etc.) and minus signs are not allowed. These types of
  transformations should be enacted using `step` functions in this
  package. Dots are allowed as are simple multivariate outcome terms
  (i.e. no need for [`cbind()`](https://rdrr.io/r/base/cbind.html); see
  Examples). A model formula may not be the best choice for
  high-dimensional data with many columns, because of problems with
  memory.

- vars:

  A character string of column names corresponding to variables that
  will be used in any context (see below)

- roles:

  A character string (the same length of `vars`) that describes a single
  role that the variable will take. This value could be anything but
  common roles are `"outcome"`, `"predictor"`, `"case_weight"`, or
  `"ID"`.

- strings_as_factors:

  A logical, should character columns be converted to factors? See
  Details below.

## Value

An object of class `recipe` with sub-objects:

- var_info:

  A tibble containing information about the original data set columns.

- term_info:

  A tibble that contains the current set of terms in the data set. This
  initially defaults to the same data contained in `var_info`.

- steps:

  A list of `step` or `check` objects that define the sequence of
  preprocessing operations that will be applied to data. The default
  value is `NULL`.

- template:

  A tibble of the data. This is initialized to be the same as the data
  given in the `data` argument but can be different after the recipe is
  trained.

## Details

### Defining recipes

Creating a recipe comes in two parts:

1.  Specifying which variables are used and what roles they should have.

2.  Specifying what transformations should be applied to which
    variables.

The first part is done with `recipe()` and optionally
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
[`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
and
[`remove_role()`](https://recipes.tidymodels.org/dev/reference/roles.md).
A `recipe` object can be created in several ways. If an analysis only
contains outcomes and predictors, the simplest way to create one is to
use a formula (e.g. `y ~ x1 + x2`) that does not contain inline
functions such as `log(x3)`.

    recipe(data, formula)
    recipe(formula, data)

Variables in recipes can have any type of *role*, including outcome,
predictor, observation ID, case weights, stratification variables, etc.
You can instead use the `vars` and `roles` argument to specify the
variables and roles. `vars` must be a character vector of names and
`roles` must the the corresponding roles.

    recipe(data, vars = vars, roles = roles)

Lastly you can use
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
[`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
and
[`remove_role()`](https://recipes.tidymodels.org/dev/reference/roles.md).
These functions will alter, add, or eliminate roles from the selections.
These can be used in combination with the above ways, or by themselves
since `recipe(data)` will consume all the data as undeclared roles. Note
that
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
[`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
and
[`remove_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
are applied before steps and checks, regardless of where they are in the
pipeline.

    recipe(data) |>
      update_role(class, new_role = "outcome") |>
      update_role(starts_with("x"), new_role = "predictor")

There are two different types of operations that can be sequentially
added to a recipe.

- **Steps** can include operations like scaling a variable, creating
  dummy variables or interactions, and so on. More computationally
  complex actions such as dimension reduction or imputation can also be
  specified.

- **Checks** are operations that conduct specific tests of the data.
  When the test is satisfied, the data are returned without issue or
  modification. Otherwise, an error is thrown.

If you have defined a recipe and want to see which steps are included,
use the
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
method on the recipe object.

Note that the data passed to `recipe()` need not be the complete data
that will be used to train the steps (by
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)). The
recipe only needs to know the names and types of data that will be used.
For large data sets, [`head()`](https://rdrr.io/r/utils/head.html) could
be used to pass a smaller data set to save time and memory.

### Using recipes

Once a recipe is defined, it needs to be *estimated* before being
applied to data. Most recipe steps have specific quantities that must be
calculated or estimated. For example,
[`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md)
needs to compute the training set’s mean for the selected columns, while
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
needs to determine the factor levels of selected columns in order to
make the appropriate indicator columns.

The two most common application of recipes are modeling and stand-alone
preprocessing. How the recipe is estimated depends on how it is being
used.

#### Modeling

The best way to use use a recipe for modeling is via the `workflows`
package. This bundles a model and preprocessor (e.g. a recipe) together
and gives the user a fluent way to train the model/recipe and make
predictions.

    library(dplyr)
    library(workflows)
    library(recipes)
    library(parsnip)

    data(biomass, package = "modeldata")

    # split data
    biomass_tr <- biomass |> filter(dataset == "Training")
    biomass_te <- biomass |> filter(dataset == "Testing")

    # With only predictors and outcomes, use a formula:
    rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
                  data = biomass_tr)

    # Now add preprocessing steps to the recipe:
    sp_signed <-
      rec |>
      step_normalize(all_numeric_predictors()) |>
      step_spatialsign(all_numeric_predictors())
    sp_signed

    ##

    ## -- Recipe ------------------------------------------------------------

    ##

    ## -- Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 5

    ##

    ## -- Operations

    ## * Centering and scaling for: all_numeric_predictors()

    ## * Spatial sign on: all_numeric_predictors()

We can create a `parsnip` model, and then build a workflow with the
model and recipe:

    linear_mod <- linear_reg()

    linear_sp_sign_wflow <-
      workflow() |>
      add_model(linear_mod) |>
      add_recipe(sp_signed)

    linear_sp_sign_wflow

    ## == Workflow ==========================================================
    ## Preprocessor: Recipe
    ## Model: linear_reg()
    ##
    ## -- Preprocessor ------------------------------------------------------
    ## 2 Recipe Steps
    ##
    ## * step_normalize()
    ## * step_spatialsign()
    ##
    ## -- Model -------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ##
    ## Computational engine: lm

To estimate the preprocessing steps and then fit the linear model, a
single call to [`fit()`](https://generics.r-lib.org/reference/fit.html)
is used:

    linear_sp_sign_fit <- fit(linear_sp_sign_wflow, data = biomass_tr)

When predicting, there is no need to do anything other than call
[`predict()`](https://parsnip.tidymodels.org/reference/predict.model_fit.html).
This preprocesses the new data in the same manner as the training set,
then gives the data to the linear model prediction code:

    predict(linear_sp_sign_fit, new_data = head(biomass_te))

    ## # A tibble: 6 x 1
    ##   .pred
    ##   <dbl>
    ## 1  18.1
    ## 2  17.9
    ## 3  17.2
    ## 4  18.8
    ## 5  19.6
    ## 6  14.6

#### Stand-alone use of recipes

When using a recipe to generate data for a visualization or to
troubleshoot any problems with the recipe, there are functions that can
be used to estimate the recipe and apply it to new data manually.

Once a recipe has been defined, the
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
function can be used to estimate quantities required for the operations
using a data set (a.k.a. the training data).
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) returns
a recipe.

As an example of using PCA (perhaps to produce a plot):

    # Define the recipe
    pca_rec <-
      rec |>
      step_normalize(all_numeric_predictors()) |>
      step_pca(all_numeric_predictors())

Now to estimate the normalization statistics and the PCA loadings:

    pca_rec <- prep(pca_rec, training = biomass_tr)
    pca_rec

    ##

    ## -- Recipe ------------------------------------------------------------

    ##

    ## -- Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 5

    ##

    ## -- Training information

    ## Training data contained 456 data points and no incomplete rows.

    ##

    ## -- Operations

    ## * Centering and scaling for: carbon hydrogen, ... | Trained

    ## * PCA extraction with: carbon, hydrogen, oxygen, ... | Trained

Note that the estimated recipe shows the actual column names captured by
the selectors.

You can
[`tidy.recipe()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
a recipe, either when it is prepped or unprepped, to learn more about
its components.

    tidy(pca_rec)

    ## # A tibble: 2 x 6
    ##   number operation type      trained skip  id
    ##    <int> <chr>     <chr>     <lgl>   <lgl> <chr>
    ## 1      1 step      normalize TRUE    FALSE normalize_AeYA4
    ## 2      2 step      pca       TRUE    FALSE pca_Zn1yz

You can also
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
recipe *steps* with a `number` or `id` argument.

To apply the prepped recipe to a data set, the
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
function is used in the same manner that
[`predict()`](https://parsnip.tidymodels.org/reference/predict.model_fit.html)
would be for models. This applies the estimated steps to any data set.

    bake(pca_rec, head(biomass_te))

    ## # A tibble: 6 x 6
    ##     HHV    PC1    PC2     PC3     PC4     PC5
    ##   <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
    ## 1  18.3 0.730  -0.412 -0.495   0.333   0.253
    ## 2  17.6 0.617   1.41   0.118  -0.466   0.815
    ## 3  17.2 0.761   1.10  -0.0550 -0.397   0.747
    ## 4  18.9 0.0400  0.950  0.158   0.405  -0.143
    ## 5  20.5 0.792  -0.732  0.204   0.465  -0.148
    ## 6  18.5 0.433  -0.127 -0.354  -0.0168 -0.0888

In general, the workflow interface to recipes is recommended for most
applications.

### Strings and Factors

The primary purpose of a recipe is to facilitate visualization,
modeling, and analysis. Because of this, most qualitative data should be
encoded as factors instead of character strings (with exceptions for
text analysis and related tasks). It is preferred that quantitative data
be converted to factors prior to passing the data to the recipe since
the number of levels is usually required for steps (e.g., for making
dummy indicator columns).

Although it is advisable to create factors before calling `recipe()`,
that function has a `strings_as_factors` argument that can do the
conversion. This affects the preprocessed training set (when
`retain = TRUE`) as well as the results of both
[`prep.recipe()`](https://recipes.tidymodels.org/dev/reference/prep.md)
and
[`bake.recipe()`](https://recipes.tidymodels.org/dev/reference/bake.md).
This will only affect variables with roles `"outcome"` and `"predictor"`

In 1.2.1 and prior versions of the recipes package, this argument was
provided via
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md). Code
that only provides it via
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) will
continue to work with a once-per-session warning, and in a future
version, it will become an error. If provided in both
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
`recipe()`, the value in `recipe()` will take precedence. Default to
`NULL,` which will be taken as `TRUE`.

## See also

[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)

## Examples

``` r
# formula example with single outcome:
data(biomass, package = "modeldata")

# split data
biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

# With only predictors and outcomes, use a formula
rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

# Now add preprocessing steps to the recipe
sp_signed <- rec |>
  step_normalize(all_numeric_predictors()) |>
  step_spatialsign(all_numeric_predictors())
sp_signed
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 5
#> 
#> ── Operations 
#> • Centering and scaling for: all_numeric_predictors()
#> • Spatial sign on: all_numeric_predictors()

# formula multivariate example:
# no need for `cbind(carbon, hydrogen)` for left-hand side

multi_y <- recipe(carbon + hydrogen ~ oxygen + nitrogen + sulfur,
  data = biomass_tr
)
multi_y <- multi_y |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors())

# example using `update_role` instead of formula:
# best choice for high-dimensional data

rec <- recipe(biomass_tr) |>
  update_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
    new_role = "predictor"
  ) |>
  update_role(HHV, new_role = "outcome") |>
  update_role(sample, new_role = "id variable") |>
  update_role(dataset, new_role = "splitting indicator")
rec
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:             1
#> predictor:           5
#> id variable:         1
#> splitting indicator: 1
```
