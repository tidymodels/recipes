# recipes 0.1.17

## New Steps

* Added new `step_harmonic()` (#702).

* Added a new step called `step_dummy_multi_choice()`, which will take multiple nominal variables and produces shared dummy variables. (#716)

## Deprecation News

* The deprecation for `step_upsample()` and `step_downsample()` has been escalated from a deprecation warning to a deprecation error; these functions are available in the themis package.

* Escalate deprecation for old versions of imputation steps (such as `step_bagimpute()`) from a soft deprecation to a regular deprecation; these imputation steps have new names like `step_impute_bag()` (#753).

* `step_kpca()` was un-deprecated and gained the `keep_original_cols` argument.

* The deprecation of the `preserve` argument to `step_pls()` and `step_dummy()` was escalated from a soft deprecation to regular deprecation. 

* The deprecation of the `options` argument to `step_nzv()` was escalated to a deprecation error.

## Bug Fixes

* Fix imputation steps for new data that is all `NA`, and generate a warning for recipes created under previous versions that cannot be imputed with this fix (#719).

* A bug was fixed where imputed values via bagged trees would have the wrong levels.

## Improvements and Other Changes

* The computations for the Yeo-Johnson transformation were made more efficient (#782).

* New `recipes_eval_select()` which is a developer tool that is useful for creating new recipes steps. It powers the tidyselect semantics that are specific to recipes and supports the modern tidyselect API introduced in tidyselect 1.0.0. Additionally, the older `terms_select()` has been deprecated in favor of this new helper (#739).

* Speed-up/simplification to `step_spatialsign()`

* When only the terms attributes are desired from `model.frame` use the first row of data to improve speed and memory use (#726).

* Use Haversine formula for latitude-longitude pairs in `step_geodist()` (#725).

* Reorganize documentation for all recipe step `tidy` methods (#701).

* Generate warning when user attempts a Box-Cox transformation of non-positive data (@LiamBlake, #713).

* `step_logit()` gained an offset argument for cases where the input is either zero or one (#784)

* The `tidy()` methods for objects from `check_new_values()`, `check_class()` and `step_nnmf()` are now exported.


# recipes 0.1.16

## New Steps

* Added a new step called `step_indicate_na()`, which will create and append additional binary columns to the data set to indicate which observations are missing (#623).

* Added new `step_select()` (#199).

## Bug Fixes

* The `threshold` argument of `step_pca()` is now `tunable()` (#534).

* Integer variables used in `step_profile()` are now kept as integers (and not doubles). 

* Preserve multiple roles in `last_term_info` so `bake()` can correctly respond to `has_roles`. (#632)

* Fixed behavior of the retain flag in `prep()` (#652).

* The `tidy()` methods for `step_nnmf()` was rewritten since it was not great (#665), and `step_nnmf()` now no longer fully loads underlying packages (#685). 

## Improvements and Other Changes

* Two new selectors that combine role and data type were added: `all_numeric_predictors()` and `all_nominal_predictors()`. (#620)

* Changed the names of all imputation steps, for example, from `step_knnimpute()` or `step_medianimpute()` (old) to `step_impute_knn()` or `step_impute_median()` (new) (#614).

* Added `keep_original_cols` argument to several steps: 
  * `step_pca()`, `step_ica()`, `step_nnmf()`, `step_kpca_rbf()`, `step_kpca_poly()`, `step_pls()`, `step_isomap()` which all default to `FALSE` (#635).
  * `step_ratio()`, `step_holiday()`, `step_date()` which all default to `TRUE` to maintain original behavior, as well as `step_dummy()` which defaults to `FALSE` (#645).

* Added `allow_rename` argument to `recipes_eval_select()` (#646).

* Performance improvements for `step_bs()` and `step_ns()`. The `prep()` step no longer evaluates the basis functions on the training set and the `bake()` steps only evaluates the basis functions once for each unique input value (#574)

* The `neighbors` parameter's default range for `step_isomap()` was changed to be 20-80.

* The deprecation for `step_upsample()` and `step_downsample()` has been escalated from a soft deprecation to a regular deprecation; these functions are available in the themis package.  

* Re-licensed package from GPL-2 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/recipes/issues/670).

# recipes 0.1.15

* The full tidyselect DSL is now allowed inside recipes `step_*()` functions. This includes the operators `&`, `|`, `-` and `!` and the new `where()` function. Additionally, the restriction preventing user defined selectors from being used has been lifted (#572).

* If steps that drop/add variables are skipped when baking the test set, the resulting column ordering of the baked test set will now be relative to the original recipe specification rather than relative to the baked training set. This is often more intuitive.

* More infrastructure work to make parallel processing on Windows less buggy with PSOCK clusters

* `fully_trained()` now returns `FALSE` when an unprepped recipe is used. 

# recipes 0.1.14

* `prep()` gained an option to print a summary of which columns were added and/or removed during execution. 

* To reduce confusion between `bake()` and `juice()`, the latter is superseded in favor of using `bake(object, new_data = NULL)`. The `new_data` argument now has no default, so a `NULL` value must be explicitly used in order to emulate the results of `juice()`. `juice()` will remain in the package (and used internally) but most communication and training will use  `bake(object, new_data = NULL)`. (#543)

* Tim Zhou added a step to use linear models for imputation (#555)

# recipes 0.1.13

## Breaking Changes

* `step_filter()`, `step_slice()`, `step_sample()`, and `step_naomit()` had their defaults for `skip` changed to `TRUE`. In the vast majority of applications, these steps should not be applied to the test or assessment sets. 

* `tidyr` version 1.0.0 or later is now required. 

## Other Changes

* `step_pls()` was changed so that it uses the Bioconductor mixOmics package. Objects created with previous versions of `recipes` can still use `juice()` and `bake()`. With the current version, the categorical outcomes can be used but now multivariate models do not. Also, the new method allows for sparse results. 

* As suggested by @StefanBRas, `step_ica()` now defaults to the C engine (#518)

* Avoided partial matching on `seq()` arguments in internal functions. 

* Improved error messaging, for example when a user tries to `prep()` a tuneable recipe.

* `step_upsample()` and `step_downsample()` are soft deprecated in recipes as they are now available in the themis package. They will be removed in the next version. 

* `step_zv()` now handles `NA` values so that variables with zero variance _plus_ are removed.

* The selectors `all_of()` and `any_of()` can now be used in step selections (#477).

* The `tune` pacakge can now use recipes with `check` operations (but also requires `tune` >= 0.1.0.9000).

* The `tidy` method for `step_pca()` now has an option for returning the variance statistics for each component.


# recipes 0.1.12

* Some S3 methods were not being registered previously. This caused issues in R 4.0. 

# recipes 0.1.11

## Other Changes

* While `recipes` does not directly depend on `dials`, it has several S3 methods for generics in `dials`. Version 0.0.5 of `dials` added stricter validation for these methods, so changes were required for `recipes`.  

## New Operations

* `step_cut()` enables you to create a factor from a numeric based on provided break (contributed by Edwin Thoen)

# recipes 0.1.10

## Breaking Changes

* renamed `yj_trans()` to `yj_transform()` to avoid conflicts. 

## Other Changes

* Added flexible naming options for new columns created by `step_depth()` and `step_classdist()` (#262).

* Small changes for base R's `stringsAsFactors` change. 

# recipes 0.1.9

 * Delayed S3 method registration for `tune::tunable()` methods that live in recipes will now work correctly on R >=4.0.0 ([#439](https://github.com/tidymodels/recipes/issues/439), [tidymodels/tune#146](https://github.com/tidymodels/tune/issues/146)).
 
 * `step_relevel()` added.

#  `recipes` 0.1.8

## Breaking Changes

 * The imputation steps do not change the data type being imputed now. Previously, if the data were integer, the data would be changed to numeric (for some step types). The change is breaking since the underlying data of imputed values are now saved as a list instead of a vector (for some step types). 
 
 * The data sets were moved to the new `modeldata` package. 
 
 * `step_num2factor()` was rewritten due to a bug that ignored the user-supplied levels ([#425](https://github.com/tidymodels/recipes/issues/425)). The results of the `transform` argument are now required to be a function and `levels` must now be supplied. 

## Other Changes

 * Using a minus in the formula to `recipes()` is no longer allowed (it didn't remove variables anyway). `step_rm()` or `update_role()` can be used instead. 

 * When using a selector that returns no columns, `juice()` and `bake()` will now return a tibble with as many rows as the original template data or the `new_data` respectively. This is more consistent with how selectors work in dplyr ([#411](https://github.com/tidymodels/recipes/issues/411)).
 
 * Code was added to explicitly register `tunable` methods when `recipes` is loaded. This is required because of changes occurring in R 4.0. 
 
* `check_class()` checks if a variable is of the designated class. Class is either learned from the train set or provided in the check. (contributed by Edwin Thoen)

* `step_normalize()` and `step_scale()` gained a `factor` argument with values of 1 or 2 that can scale the standard deviations used to transform the data. ([#380](https://github.com/tidymodels/recipes/issues/380))

* `bake()` now produces a tibble with columns in the same order as `juice()` ([#365](https://github.com/tidymodels/recipes/issues/365))

#  `recipes` 0.1.7

Release driven by changes in `tidyr` (v 1.0.0). 

## Breaking Changes

`format_selector()`'s `wdth` argument has been renamed to `width` 
([#250](https://github.com/tidymodels/recipes/issues/250)).

## New Operations

 * `step_mutate_at()`, `step_rename()`, and `step_rename_at()` were added. 

## Other Changes

 * The use of `varying()` will be deprecated in favor of an upcoming function `tune()`. No changes are need in this version, but subsequent versions will work with `tune()`.

 * `format_ch_vec()` and `format_selector()` are now exported ([#250](https://github.com/tidymodels/recipes/issues/250)).

 * `check_new_values` breaks `bake` if variable contains values that were not observed in the train set (contributed by Edwin Thoen)
 
 * When no outcomes are in the recipe, using `juice(object, all_outcomes()` and `bake(object, new_data, all_outcomes()` will return a tibble with zero rows and zero columns (instead of failing). ([#298](https://github.com/tidymodels/recipes/issues/298)). This will also occur when the selectors select no columns. 

 * As alternatives to `step_kpca()`, two separate steps were added called `step_kpca_rbf()` and `step_kpca_poly()`. The use of `step_kpca()` will print a deprecation message that it will be going away.

 * `step_nzv()` and `step_poly()` had arguments promoted out of their `options` slot. `options` can be used in the short term but is deprecated.

 * `step_downsample()` will replace the `ratio` argument with `under_ratio` and `step_upsample()` will replace it with `over_ratio`. `ratio` still works (for now) but issues a deprecation message.

 * `step_discretize()` has arguments moved out of `options` too; the main arguments are now `num_breaks` (instead of `cuts`) and `min_unique`. Again, deprecation messages are issued with the old argument structure. 

 * Models using the `dimRed` package (`step_kpca()`, `step_isomap()`, and `step_nnmf()`) would silently fail if the projection method failed. An error is issued now. 

 * Methods were added for a future generic called `tunable()`. This outlines which parameters in a step can/could be tuned. 


# `recipes` 0.1.6

Release driven by changes in `rlang`. 

## Breaking Changes

 * Since 2018, a warning has been issued when the wrong argument was used in `bake(recipe, newdata)`. The depredation period is over and `new_data` is officially required.  

 * Previously, if `step_other()` did _not_ collapse any levels, it would still add an "other" level to the factor. This would lump new factor levels into "other" when data were baked (as `step_novel()` does). This no longer occurs since it was inconsistent with `?step_other`, which said that 

 > "If no pooling is done the data are unmodified".

## New Operations

* `step_normalize()` centers and scales the data (if you are, like Max, too lazy to use two separate steps). 
* `step_unknown()` will convert missing data in categorical columns to "unknown" and update factor levels. 
 
## Other Changes

* If `threshold` argument of `step_other` is greater than one then it specifies the minimum sample size before the levels of the factor are collapsed into the "other" category. [#289](https://github.com/tidymodels/recipes/issues/289)


 * `step_knnimpute()` can now pass two options to the underlying knn code, including the number of threads ([#323](https://github.com/tidymodels/recipes/issues/323)). 

* Due to changes by CRAN, `step_nnmf()` only works on versions of R >= 3.6.0 due to dependency issues. 

* `step_dummy()` and `step_other()` are now tolerant to cases where that step's selectors do not capture any columns. In this case, no modifications to the data are made. ([#290](https://github.com/tidymodels/recipes/issues/290), [#348](https://github.com/tidymodels/recipes/issues/348))

* `step_dummy()` can now retain the original columns that are used to make the dummy variables. ([#328](https://github.com/tidymodels/recipes/issues/328)) 

* `step_other()`'s print method only reports the variables with collapsed levels (as opposed to any column that was _tested_ to see if it needed collapsing). ([#338](https://github.com/tidymodels/recipes/issues/338)) 

 * `step_pca()`, `step_kpca()`, `step_ica()`, `step_nnmf()`, `step_pls()`, and `step_isomap()` now accept zero components. In this case, the original data are returned. 
 

# `recipes` 0.1.5

Small release driven by changes in `sample()` in the current r-devel. 

## Other Changes

* A new vignette discussing roles has been added.

* To provide infrastructure for finalizing varying parameters, an `update()` method for recipe steps has been added. This allows users to alter information in steps that have not yet been trained.

* `step_interact` will no longer fail if an interaction contains an interaction using column that has been previously filtered from the data. A warning is issued when this happens and no interaction terms will be created.

* `step_corr` was made more fault tolerant for cases where the data contain a zero-variance column or columns with missing values.

* Set the embedded environment to NULL in `prep.step_dummy` to reduce the file size of serialized recipe class objects when using `saveRDS`.
 
## Breaking Changes

* The `tidy` method for `step_dummy` now returns the original variable _and_ the levels of the future dummy variables. 

## Bug Fixes

* Updating the role of new columns generated by a recipe step no longer also updates `NA` roles of existing columns ([#296](https://github.com/tidymodels/recipes/issues/296)).

# `recipes` 0.1.4

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



## New Operations

 * `step_integer` converts data to ordered integers similar to [`LabelEncoder`](https://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.LabelEncoder.html) [#123](https://github.com/tidymodels/recipes/issues/123) and [#185](https://github.com/tidymodels/recipes/issues/185)
 * `step_geodist` can be used to calculate the distance between geocodes and a single reference location. 
 * `step_arrange`, `step_filter`, `step_mutate`, `step_sample`, and `step_slice` implement their `dplyr` analogs. 
 * `step_nnmf` computes the non-negative matrix factorization for data. 


## Other Changes

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

## New Operations

* `check_range` breaks `bake` if variable range in new data is outside the range that was learned from the train set (contributed by Edwin Thoen)
* `step_lag` can lag variables in the data set (contributed by Alex Hayes).

* `step_naomit` removes rows with missing data for specific columns (contributed by Alex Hayes). 

* `step_rollimpute` can be used to impute data in a sequence or series by estimating their values within a moving window. 

* `step_pls` can conduct supervised feature extraction for predictors. 

## Other Changes

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

## Bug Fixes
  
 * [issue 125](https://github.com/tidymodels/recipes/issues/125) that prevented several steps from working with **dplyr** grouped data frames. (contributed by Jeffrey Arnold)
 
 *  [issue 127](https://github.com/tidymodels/recipes/issues/127) where options to `step_discretize` were not being passed to `discretize`.

# `recipes` 0.1.2

## General Changes

* Edwin Thoen suggested [adding validation checks](https://github.com/tidymodels/recipes/pull/104) for certain data characteristics. This fed into the existing notion of expanding `recipes` beyond steps (see the [non-step steps project](https://github.com/tidymodels/recipes/projects)). A new set of operations, called **`checks`**, can now be used. These should throw an informative error when the check conditions are not met and return the existing data otherwise. 

* Steps now have a `skip` option that will not apply preprocessing when `bake` is used. See the article [on skipping steps](https://recipes.tidymodels.org/articles/Skipping.html) for more information. 


## New Operations

 * `check_missing` will validate that none of the specified variables contain missing data. 
 
 * `detect_step` can be used to check if a recipe contains a particular preprocessing operation.
 
 * `step_num2factor` can be used to convert numeric data (especially integers) to factors. 
 
 * `step_novel` adds a new factor level to nominal variables that will be used when new data contain a level that did not exist when the recipe was prepared. 
 
 * `step_profile` can be used to generate design matrix grids for prediction profile plots of additive models where one variable is varied over a grid and all of the others are fixed at a single value. 
 
 * `step_downsample` and `step_upsample` can be used to change the number of rows in the data based on the frequency distributions of a factor variable in the training set. By default, this operation is only applied to the training set; `bake` ignores this operation.
 
 * `step_naomit` drops rows when specified columns contain `NA`, similar to `tidyr::drop_na`.
 
 * `step_lag` allows for the creation of lagged predictor columns.

## Other Changes

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

## New steps

  * `step_lincomb` removes variables involved in linear combinations to resolve them. 
  * A step for converting binary variables to factors (`step_bin2factor`)
  *  `step_regex` applies a regular expression to a character or factor vector to create dummy variables. 

## Other changes 

* `step_dummy` and `step_interact` do a better job of respecting missing values in the data set. 


# `recipes` 0.0.1.9001

* The class system for `recipe` objects was changed so that [pipes can be used to create the recipe with a formula](https://github.com/tidymodels/recipes/issues/46).
* `process.recipe` lost the `role` argument in factor of a general set of [selectors](https://recipes.tidymodels.org/articles/Selecting_Variables.html). If no selector is used, all the predictors are returned. 
* Two steps for simple imputation using the mean or mode were added. 
