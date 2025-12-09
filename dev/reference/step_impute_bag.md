# Impute via bagged trees

`step_impute_bag()` creates a *specification* of a recipe step that will
create bagged tree models to impute missing data.

## Usage

``` r
step_impute_bag(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  impute_with = all_predictors(),
  trees = 25,
  models = NULL,
  options = list(keepX = FALSE),
  seed_val = sample.int(10^4, 1),
  skip = FALSE,
  id = rand_id("impute_bag")
)

imp_vars(...)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables to be imputed. When
  used with `imp_vars`, these dots indicate which variables are used to
  predict the missing data in each variable. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- impute_with:

  Bare names or selectors functions that specify which variables are
  used to impute the variables that can include specific variable names
  separated by commas or different selectors (see
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)).
  If a column is included in both lists to be imputed and to be an
  imputation predictor, it will be removed from the latter and not used
  to impute itself.

- trees:

  An integer for the number of bagged trees to use in each model.

- models:

  The [`ipred::ipredbagg()`](https://rdrr.io/pkg/ipred/man/bagging.html)
  objects are stored here once this bagged trees have be trained by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- options:

  A list of options to
  [`ipred::ipredbagg()`](https://rdrr.io/pkg/ipred/man/bagging.html).
  Defaults are set for the arguments `nbagg` and `keepX` but others can
  be passed in. **Note** that the arguments `X` and `y` should not be
  passed here.

- seed_val:

  An integer used to create reproducible models. The same seed is used
  across all imputation models.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

For each variable requiring imputation, a bagged tree is created where
the outcome is the variable of interest and the predictors are any other
variables listed in the `impute_with` formula. One advantage to the
bagged tree is that is can accept predictors that have missing values
themselves. This imputation method can be used when the variable of
interest (and predictors) are numeric or categorical. Imputed
categorical variables will remain categorical. Also, integers will be
imputed to integer too.

Note that if a variable that is to be imputed is also in `impute_with`,
this variable will be ignored.

It is possible that missing values will still occur after imputation if
a large majority (or all) of the imputing variables are also missing.

As of `recipes` 0.1.16, this function name changed from
[`step_bagimpute()`](https://recipes.tidymodels.org/dev/reference/step_bagimpute.md)
to `step_impute_bag()`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `model` , and
`id`:

- terms:

  character, the selectors or variables selected

- model:

  list, the bagged tree object

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `trees`: \# Trees (type: integer, default: 25)

## Case weights

The underlying operation does not allow for case weights.

## References

Kuhn, M. and Johnson, K. (2013). *Applied Predictive Modeling*. Springer
Verlag.

## See also

Other imputation steps:
[`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md),
[`step_impute_linear()`](https://recipes.tidymodels.org/dev/reference/step_impute_linear.md),
[`step_impute_lower()`](https://recipes.tidymodels.org/dev/reference/step_impute_lower.md),
[`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md),
[`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md),
[`step_impute_mode()`](https://recipes.tidymodels.org/dev/reference/step_impute_mode.md),
[`step_impute_roll()`](https://recipes.tidymodels.org/dev/reference/step_impute_roll.md)

## Examples

``` r
data("credit_data", package = "modeldata")

## missing data per column
vapply(credit_data, function(x) mean(is.na(x)), c(num = 0))
#>       Status    Seniority         Home         Time          Age 
#> 0.0000000000 0.0000000000 0.0013471037 0.0000000000 0.0000000000 
#>      Marital      Records          Job     Expenses       Income 
#> 0.0002245173 0.0000000000 0.0004490346 0.0000000000 0.0855410867 
#>       Assets         Debt       Amount        Price 
#> 0.0105523125 0.0040413112 0.0000000000 0.0000000000 

set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[in_training, ]
credit_te <- credit_data[-in_training, ]
missing_examples <- c(14, 394, 565)

rec <- recipe(Price ~ ., data = credit_tr)
if (FALSE) { # \dontrun{
impute_rec <- rec |>
  step_impute_bag(Status, Home, Marital, Job, Income, Assets, Debt)

imp_models <- prep(impute_rec, training = credit_tr)

imputed_te <- bake(imp_models, new_data = credit_te)

credit_te[missing_examples, ]
imputed_te[missing_examples, names(credit_te)]

tidy(impute_rec, number = 1)
tidy(imp_models, number = 1)

## Specifying which variables to imputate with

impute_rec <- rec |>
  step_impute_bag(Status, Home, Marital, Job, Income, Assets, Debt,
    impute_with = c(Time, Age, Expenses),
    # for quick execution, nbagg lowered
    options = list(nbagg = 5, keepX = FALSE)
  )

imp_models <- prep(impute_rec, training = credit_tr)

imputed_te <- bake(imp_models, new_data = credit_te)

credit_te[missing_examples, ]
imputed_te[missing_examples, names(credit_te)]

tidy(impute_rec, number = 1)
tidy(imp_models, number = 1)
} # }
```
