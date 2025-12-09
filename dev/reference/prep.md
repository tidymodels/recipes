# Estimate a preprocessing recipe

For a recipe with at least one preprocessing operation, estimate the
required parameters from a training set that can be later applied to
other data sets.

## Usage

``` r
prep(x, ...)

# S3 method for class 'recipe'
prep(
  x,
  training = NULL,
  fresh = FALSE,
  verbose = FALSE,
  retain = TRUE,
  log_changes = FALSE,
  strings_as_factors = TRUE,
  ...
)
```

## Arguments

- x:

  an
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  object.

- ...:

  further arguments passed to or from other methods (not currently
  used).

- training:

  A data frame, tibble, or sparse matrix from the `Matrix` package, that
  will be used to estimate parameters for preprocessing. See
  [sparse_data](https://recipes.tidymodels.org/dev/reference/sparse_data.md)
  for more information about use of sparse data.

- fresh:

  A logical indicating whether already trained operation should be
  re-trained. If `TRUE`, you should pass in a data set to the argument
  `training`.

- verbose:

  A logical that controls whether progress is reported as operations are
  executed.

- retain:

  A logical: should the *preprocessed* training set be saved into the
  `template` slot of the recipe after training? This is a good idea if
  you want to add more steps later but want to avoid re-training the
  existing steps. Also, it is advisable to use `retain = TRUE` if any
  steps use the option `skip = FALSE`. **Note** that this can make the
  final recipe size large. When `verbose = TRUE`, a message is written
  with the approximate object size in memory but may be an underestimate
  since it does not take environments into account.

- log_changes:

  A logical for printing a summary for each step regarding which (if
  any) columns were added or removed during training.

- strings_as_factors:

  A logical: should character columns that have role `"predictor"` or
  `"outcome"` be converted to factors? **This option has now been moved
  to
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)**;
  please specify `strings_as_factors` there and see the notes in the
  Details section for that function.

## Value

A recipe whose step objects have been updated with the required
quantities (e.g. parameter estimates, model objects, etc). Also, the
`term_info` object is likely to be modified as the operations are
executed.

## Details

Given a data set, this function estimates the required quantities and
statistics needed by any operations. `prep()` returns an updated recipe
with the estimates. If you are using a recipe as a preprocessor for
modeling, we **highly recommend** that you use a `workflow()` instead of
manually estimating a recipe (see the example in
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)).

Note that missing data is handled in the steps; there is no global
`na.rm` option at the recipe level or in `prep()`.

Also, if a recipe has been trained using `prep()` and then steps are
added, `prep()` will only update the new operations. If `fresh = TRUE`,
all of the operations will be (re)estimated.

As the steps are executed, the `training` set is updated. For example,
if the first step is to center the data and the second is to scale the
data, the step for scaling is given the centered data.

## See also

[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md) and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)

## Examples

``` r
data(ames, package = "modeldata")

library(dplyr)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

ames_rec <-
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood + Year_Built + Central_Air,
    data = ames
  ) |>
  step_other(Neighborhood, threshold = 0.05) |>
  step_dummy(all_nominal()) |>
  step_interact(~ starts_with("Central_Air"):Year_Built) |>
  step_ns(Longitude, Latitude, deg_free = 5)

prep(ames_rec, verbose = TRUE)
#> oper 1 step other [training] 
#> oper 2 step dummy [training] 
#> oper 3 step interact [training] 
#> oper 4 step ns [training] 
#> The retained training set is ~ 0.48 Mb  in memory.
#> 
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 5
#> 
#> ── Training information 
#> Training data contained 2930 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Collapsing factor levels for: Neighborhood | Trained
#> • Dummy variables from: Neighborhood Central_Air | Trained
#> • Interactions with: Central_Air_Y:Year_Built | Trained
#> • Natural splines on: Longitude Latitude | Trained

prep(ames_rec, log_changes = TRUE)
#> step_other (other_slYqj): same number of columns
#> 
#> step_dummy (dummy_dHOrD): 
#>  new (9): Neighborhood_College_Creek, Neighborhood_Old_Town, ...
#>  removed (2): Neighborhood, Central_Air
#> 
#> step_interact (interact_SrwZQ): 
#>  new (1): Central_Air_Y_x_Year_Built
#> 
#> step_ns (ns_SSy0I): 
#>  new (10): Longitude_ns_1, Longitude_ns_2, Longitude_ns_3, ...
#>  removed (2): Longitude, Latitude
#> 
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 5
#> 
#> ── Training information 
#> Training data contained 2930 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Collapsing factor levels for: Neighborhood | Trained
#> • Dummy variables from: Neighborhood Central_Air | Trained
#> • Interactions with: Central_Air_Y:Year_Built | Trained
#> • Natural splines on: Longitude Latitude | Trained
```
