# On skipping steps

When steps are created in a recipe, they can be applied to data
(i.e. baked) at two distinct times:

1.  During the process of preparing the recipe, each step is estimated
    via `prep` and then applied to the training set using `bake` before
    proceeding to the next step.
2.  After the recipe has been prepared, `bake` can be used with any data
    set to apply the preprocessing to those data.

There are times where we would like to circumvent baking on a new data
set (i.e., \#2 above). For example:

- There are outcomes in the recipe that won’t be available when a recipe
  is baked in the future. For predictive modeling, this is common when
  you receive *new* data to be predicted.
- When doing resampling or a training/test split, certain operations
  make sense for the data to be used for modeling but are problematic
  for new samples or the test set.

## Example: Class Imbalance Sampling and Skipping Steps

As an example of the second case, consider the problem of a [severe
class imbalance](https://github.com/topepo/useR2016). Suppose that there
are two classes to be predicted and the event of interest occurs in only
5% of the time. Many models will quickly optimize accuracy by
overfitting to the majority class by predicting everything to be a
non-event. One method to compensate for this is to *down-sample* the
**training set** so that the class frequencies are about equal. Although
somewhat counter-intuitive, this can often lead to better models.

The important consideration is that this preprocessing is *only applied
to the training set* so that it can impact the model fit. The test set
should be unaffected by this operation. If the recipe is used to create
the design matrix for the model, down-sampling would remove rows. This
would be a bad idea for the test set since these data should represent
what the population of samples looks like “in the wild.”. Based on this,
a recipe that included down-sample should *skip* this step when data are
baked for the test set.

## Other Examples

There are other steps that have a default value of `skip = TRUE`:

- [`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md)
  allows for arbitrary filtering of rows.
- [`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)
  also removes (or adds) rows to the data.
- [`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md)
  enables random sampling of the data set.
- [`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md)
  will remove rows with missing values in certain columns.

The main issue with these steps being applied to new data (i.e. after
the recipe has been trained) is that the non-outcome rows can become
out-of-sync with the outcome data.

For example, when
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) is run
with `step_naomit(skip = FALSE)` and there are missing values, the
predictors will have fewer rows than the outcome vector. When model
predictions are produced and merged with the other data, their will be
discordant rows. In this case, it would be better to have missing values
in the predictions and a full data set than a subset of predictions from
the complete data. The [general
rule](https://tidymodels.github.io/model-implementation-principles/model-predictions.html)
in tidymodels is that, for models,

> The return value is a tibble with the **same number of rows** as the
> data being predicted and in the same order.

## How To Skip Steps

As of version recipes 0.1.2, each step has an optional logical argument
called `skip`. In almost every case, the default is `FALSE`. When using
this option:

- No steps are skipped during
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
- Steps with `skip = TRUE` are not applied to the data when
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) is
  called

Recall that there are two ways of getting the results for the training
set with recipes. First,
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) can be
used as usual. Second, `bake(new_data = NULL)` is a shortcut that will
use the already processed data that is contained in the recipe when
`prep(recipe, retain = TRUE)` is used. `bake(new_data = NULL)` is much
faster and would be the way to get the training set *with all of the
steps applied to the data*. For this reason, you should almost always
used `retain = TRUE` if any steps are skipped (and a warning is produced
otherwise).

## Be Careful!

Skipping is a necessary feature but can be dangerous if used carelessly.

As an example, skipping an operation whose variables are used later
might be an issue:

``` r
library(recipes)
car_recipe <- recipe(mpg ~ ., data = mtcars) |>
  step_log(disp, skip = TRUE) |>
  step_center(all_numeric_predictors()) |>
  prep(training = mtcars)

# These *should* produce the same results (as they do for `hp`)
bake(car_recipe, new_data = NULL)   |> head() |> select(disp, hp)
#> # A tibble: 6 × 2
#>     disp    hp
#>    <dbl> <dbl>
#> 1 -0.210 -36.7
#> 2 -0.210 -36.7
#> 3 -0.603 -53.7
#> 4  0.268 -36.7
#> 5  0.601  28.3
#> 6  0.131 -41.7
bake(car_recipe, new_data = mtcars) |> head() |> select(disp, hp)
#> # A tibble: 6 × 2
#>    disp    hp
#>   <dbl> <dbl>
#> 1  155. -36.7
#> 2  155. -36.7
#> 3  103. -53.7
#> 4  253. -36.7
#> 5  355.  28.3
#> 6  220. -41.7
```

This should emphasize that `bake(new_data = NULL)` should be used to get
the training set values whenever a step is skipped.
