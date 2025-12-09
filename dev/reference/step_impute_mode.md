# Impute nominal data using the most common value

`step_impute_mode()` creates a *specification* of a recipe step that
will substitute missing values of nominal variables by the training set
mode of those variables.

## Usage

``` r
step_impute_mode(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  modes = NULL,
  ptype = NULL,
  skip = FALSE,
  id = rand_id("impute_mode")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- modes:

  A named character vector of modes. This is `NULL` until computed by
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).

- ptype:

  A data frame prototype to cast new data sets to. This is commonly a
  0-row slice of the training set.

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

`step_impute_mode()` estimates the variable modes from the data used in
the `training` argument of
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) then
applies the new values to new data sets using these values. If the
training set data has more than one mode, one is selected at random.

As of `recipes` 0.1.16, this function name changed from
[`step_modeimpute()`](https://recipes.tidymodels.org/dev/reference/step_modeimpute.md)
to `step_impute_mode()`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms`, `value` , and
`id`:

- terms:

  character, the selectors or variables selected

- value:

  character, the mode value

- id:

  character, id of this step

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[case_weights](https://recipes.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

## See also

Other imputation steps:
[`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md),
[`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md),
[`step_impute_linear()`](https://recipes.tidymodels.org/dev/reference/step_impute_linear.md),
[`step_impute_lower()`](https://recipes.tidymodels.org/dev/reference/step_impute_lower.md),
[`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md),
[`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md),
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

impute_rec <- rec |>
  step_impute_mode(Status, Home, Marital)

imp_models <- prep(impute_rec, training = credit_tr)

imputed_te <- bake(imp_models, new_data = credit_te)

table(credit_te$Home, imputed_te$Home, useNA = "always")
#>          
#>           ignore other owner parents priv rent <NA>
#>   ignore      13     0     0       0    0    0    0
#>   other        0   176     0       0    0    0    0
#>   owner        0     0  1171       0    0    0    0
#>   parents      0     0     0     436    0    0    0
#>   priv         0     0     0       0  135    0    0
#>   rent         0     0     0       0    0  519    0
#>   <NA>         0     0     4       0    0    0    0

tidy(impute_rec, number = 1)
#> # A tibble: 3 × 3
#>   terms   value id               
#>   <chr>   <chr> <chr>            
#> 1 Status  NA    impute_mode_Hlm2y
#> 2 Home    NA    impute_mode_Hlm2y
#> 3 Marital NA    impute_mode_Hlm2y
tidy(imp_models, number = 1)
#> # A tibble: 3 × 3
#>   terms   value   id               
#>   <chr>   <chr>   <chr>            
#> 1 Status  good    impute_mode_Hlm2y
#> 2 Home    owner   impute_mode_Hlm2y
#> 3 Marital married impute_mode_Hlm2y
```
