# `recipes` 0.1.3.9002

## Breaking Changes

 * Several argument names were changed to be consistent with other `tidymodels` packages (e.g. `dials`) and the general tidyverse naming conventions. 
   * `K` in `step_knnimpute` was changed to `neighbors`. `step_isomap` had the number of neighbors promoted to a main argument called `neighbors ` 
   *  `step_pca`, `step_pls`, `step_kpca`, `step_ica` now use  `num_comp`  instead of `num`. , `step_isomap` uses `num_terms` instead of `num`. 
   *  `step_bagimpute` moved `nbagg` out of the options and into a main argument `trees`. 
   *  `step_bs` and `step_ns` has degrees of freedom promoted to a main argument with name `deg_free`. Also, `step_bs` had `degree` promoted to a main argument. 
   *  `step_BoxCox` and `step_YeoJohnson` had `nunique` change to `num_unique`.
   * `bake`, `juice` and other functions has `newdata` changed to `new_data`. For _this version only_, using `newdata` will only result in a wanring. 
   * Several steps had `na.rm` changed to `na_rm`.
   * `prep` and a few steps had `stringsAsFactors` changed to `strings_as_factors`. 

* `add_role()` can now only add _new_ additional roles. To alter existing roles, use `update_role()`. This change also allows for the possibility of having multiple roles/types for one variable. [#221](https://github.com/tidymodels/recipes/issues/221)

*  All steps gain an `id` field that will be used in the future to reference other steps. 

* The `retain` option to `prep` is now defaulted to `TRUE`. If `verbose = TRUE`, the approximate size of the data set is printed. [#207](https://github.com/tidymodels/recipes/issues/207)



## New Operations:

 * `step_integer` converts data to ordered integers similar to [`LabelEncoder`](http://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.LabelEncoder.html) [#123](https://github.com/tidymodels/recipes/issues/123) and [#185](https://github.com/tidymodels/recipes/issues/185)
 * `step_geodist` can be used to calculate the distance between geocodes and a single reference location. 
 * `step_arrange`, `step_filter`, `step_mutate`, `step_sample`, and `step_slice` implement their `dplyr` analogs. 
 * `step_nnmf` computes the non-negative matrix factorization for data. 


## Other Changes:

 * The `rsample` function `prepper` was moved to `recipes` [(issue)](https://github.com/tidymodels/rsample/issues/48).
 * A number of packages were moved from "Imports" to "Suggests" to reduce the install footprint. A function was added to prompt the user to install the needed packages when the relevant steps are invoked. 
 * `step_step_string2factor` will [now accept factors](https://github.com/tidymodels/recipes/issues/174) and leave them as-is. 
 * `step_knnimpute` now excludes missing data in the variable to be imputed from the nearest-neighbor calculation. This would have resulted in some missing data to not be imputed (i.e. return another missing value). 
 * `step_dummy` now produces a warning (instead of failing) when non-factor columns are selected. Only factor columns are used; no conversion is done for character data. [issue #186](https://github.com/tidymodels/recipes/issues/186)
 * `dummy_names` gained a separator argument. [issue #183](https://github.com/tidymodels/recipes/issues/183)
 * `step_downsample` and `step_upsample` now have `seed` arguments for more control over randomness. 
 * `broom` is no longer used to get the `tidy` generic. These are now contained in the `generics` package. 
 * When a recipe is prepared, a running list of all columns is created and the last known use of each column is kept. This is to avoid bugs when a step that is skipped removes columns. [issue #239](https://github.com/tidymodels/recipes/issues/239)
 

# `recipes` 0.1.3

## New Operations:

* `check_range` breaks `bake` if variable range in new data is outside the range that was learned from the train set (contributed by Edwin Thoen)
* `step_lag` can lag variables in the data set (contributed by Alex Hayes).

* `step_naomit` removes rows with missing data for specific columns (contributed by Alex Hayes). 

* `step_rollimpute` can be used to impute data in a sequence or series by estimating their values within a moving window. 

* `step_pls` can conduct supervised feature extraction for predictors. 

## Other Changes:

 * `step_log` gained an `offset` argument. 

 * `step_log` gained a `signed` argument (contributed by Edwin Thoen).
 
 * The internal functions `sel2char` and `printer` have been exported to enable [other packages to contain steps](https://github.com/tidymodels/recipes/issues/122).
 
 * When training _new_ steps after some steps have been previously trained, the `retain = TRUE` option should be set on [previous invocations of `prep`](https://github.com/tidymodels/recipes/issues/143). 
 
 * For `step_dummy`:

   * It can now compute the [entire set of dummy variables](https://github.com/tidymodels/recipes/issues/145) per factor predictor using the `one_hot = TRUE` option. Thanks to Davis Vaughan. 
   * The `contrast` option was removed. The step uses the global option for contrasts. 
   * `The step also produces missing indicator variables when the original factor [has a missing value](https://github.com/tidymodels/recipes/issues/133)
 * `step_other` will now convert novel levels of the factor to the "other" level. 
 * `step_bin2factor` now has an option to choose [how the values are translated to the levels](https://github.com/tidymodels/recipes/issues/142) (contributed by Michael Levy).
 * `bake` and `juice` can now export basic data frames. 
 * The `okc` data were updated with two additional columns. 

## Bug Fixes: 
  
 * [issue 125](https://github.com/tidymodels/recipes/issues/125) that prevented several steps from working with **dplyr** grouped data frames. (contributed by Jeffrey Arnold)
 
 *  [issue 127](https://github.com/tidymodels/recipes/issues/127) where options to `step_discretize` were not being passed to `discretize`.

# `recipes` 0.1.2

## General Changes:

* Edwin Thoen suggested [adding validation checks](https://github.com/tidymodels/recipes/pull/104) for certain data characteristics. This fed into the existing notion of expanding `recipes` beyond steps (see the [non-step steps project](https://github.com/tidymodels/recipes/projects)). A new set of operations, called **`checks`**, can now be used. These should throw an informative error when the check conditions are not met and return the existing data otherwise. 

* Steps now have a `skip` option that will not apply preprocessing when `bake` is used. See the article [on skipping steps](https://tidymodels.github.io/recipes/articles/Skipping.html) for more information. 


## New Operations:

 * `check_missing` will validate that none of the specified variables contain missing data. 
 
 * `detect_step` can be used to check if a recipe contains a particular preprocessing operation.
 
 * `step_num2factor` can be used to convert numeric data (especially integers) to factors. 
 
 * `step_novel` adds a new factor level to nominal variables that will be used when new data contain a level that did not exist when the recipe was prepared. 
 
 * `step_profile` can be used to generate design matrix grids for prediction profile plots of additive models where one variable is varied over a grid and all of the others are fixed at a single value. 
 
 * `step_downsample` and `step_upsample` can be used to change the number of rows in the data based on the frequency distributions of a factor variable in the training set. By default, this operation is only applied to the training set; `bake` ignores this operation.
 
 * `step_naomit` drops rows when specified columns contain `NA`, similar to `tidyr::drop_na`.
 
 * `step_lag` allows for the creation of lagged predictor columns.

## Other Changes:

* `step_spatialsign` now has the option of removing missing data prior to computing the norm.


# `recipes` 0.1.1

* The default selectors for `bake` was changed from `all_predictors()` to `everything()`. 
* The `verbose` option for `prep` is now defaulted to `FALSE`
* [A bug in `step_dummy`](https://github.com/tidymodels/recipes/issues/83) was fixed that makes sure that the correct binary variables are generated despite the levels or values of the incoming factor. Also, `step_dummy` now requires factor inputs.
* `step_dummy` also has a new default naming function that works better for factors. However, there is an extra argument (`ordinal`) now to the functions that can be passed to `step_dummy`.  
* `step_interact` now allows for selectors (e.g. `all_predictors()` or `starts_with("prefix")` to be used in the interaction formula. 
* `step_YeoJohnson` gained an `na.rm` option.
* [`dplyr::one_of`](https://github.com/tidymodels/recipes/issues/85) was added to the list of selectors.
* `step_bs` adds B-spline basis functions. 
* `step_unorder` converts ordered factors to unordered factors. 
* `step_count` counts the number of instances that a pattern exists in a string. 
* `step_string2factor` and `step_factor2string` can be used to move between encodings. 
* `step_lowerimpute` is for numeric data where the values cannot be measured below a specific value. For these cases, random uniform values are used for the truncated values.  
* A step to remove simple zero-variance variables was added (`step_zv`).
* A series of `tidy` methods were added for recipes and many (but not all) steps. 
* In `bake.recipe`, the argument `newdata` is now without a default. 
* `bake` and `juice` can now save the _final_ processed data set in [sparse format](https://github.com/tidymodels/recipes/issues/49). Note that, as the steps are processed, a non-sparse data frame is used to store the results. 
* A formula method was added for recipes to get a formula with the outcome(s) and predictors based on the trained recipe. 

# `recipes` 0.1.0

First CRAN release. 

* Changed `prepare` to `prep` per [issue #59](https://github.com/tidymodels/recipes/issues/59)

# `recipes` 0.0.1.9003

 * Two of the main functions [changed names](https://github.com/tidymodels/recipes/issues/57). `learn` has become `prepare` and `process` has become `bake`


# `recipes` 0.0.1.9002

## New steps:

  * `step_lincomb` removes variables involved in linear combinations to resolve them. 
  * A step for converting binary variables to factors (`step_bin2factor`)
  *  `step_regex` applies a regular expression to a character or factor vector to create dummy variables. 

## Other changes: 

* `step_dummy` and `step_interact` do a better job of respecting missing values in the data set. 


# `recipes` 0.0.1.9001

* The class system for `recipe` objects was changed so that [pipes can be used to create the recipe with a formula](https://github.com/tidymodels/recipes/issues/46).
* `process.recipe` lost the `role` argument in factor of a general set of [selectors](https://tidymodels.github.io/recipes/articles/Selecting_Variables.html). If no selector is used, all the predictors are returned. 
* Two steps for simple imputation using the mean or mode were added. 
