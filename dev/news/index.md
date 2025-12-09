# Changelog

## recipes (development version)

## recipes 1.3.1

CRAN release: 2025-05-21

- Fixed bug where
  [`tune_args()`](https://generics.r-lib.org/reference/tune_args.html)
  would error if argument to step had a parsnip object with tuned
  arguments.
  ([\#1506](https://github.com/tidymodels/recipes/issues/1506))

## recipes 1.3.0

CRAN release: 2025-04-17

### Deprecation

- [`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md)
  has started its deprecation process. See `?step_select()` for
  alternatives.
  ([\#1488](https://github.com/tidymodels/recipes/issues/1488))

- The `strings_as_factors` argument of
  [`prep.recipe()`](https://recipes.tidymodels.org/dev/reference/prep.md)
  has been soft-deprecated in favor of `recipe(strings_as_factors)`. If
  both are provided, the value in
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  takes precedence. This allows control of recipe behavior within a
  workflow, which wasn’t previously possible.
  ([@smingerson](https://github.com/smingerson),
  [\#331](https://github.com/tidymodels/recipes/issues/331),
  [\#287](https://github.com/tidymodels/recipes/issues/287))

- [`step_mutate_at()`](https://recipes.tidymodels.org/dev/reference/step_mutate_at.md)
  has been superceded in favor of
  [`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md)
  when used with
  [`across()`](https://dplyr.tidyverse.org/reference/across.html).
  ([\#662](https://github.com/tidymodels/recipes/issues/662))

### Improvements

- [`step_num2factor()`](https://recipes.tidymodels.org/dev/reference/step_num2factor.md)
  has gotten improved documentation to avoid getting NAs as output.
  ([\#575](https://github.com/tidymodels/recipes/issues/575))

- [`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md)
  now has a much smaller memory footprint when prepped.
  ([\#638](https://github.com/tidymodels/recipes/issues/638))

- The following arguments in steps can now take bare names as input
  instead of strings, calls to
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html),
  [`imp_vars()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md),
  and
  [`denom_vars()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md).
  ([\#1225](https://github.com/tidymodels/recipes/issues/1225))

  - `step_classdist_shrunken(class)`
  - `step_classdist(class)`
  - `step_depth(class)`
  - `step_impute_bag(impute_with)`
  - `step_impute_knn(impute_with)`
  - `step_impute_linear(impute_with)`
  - `step_pls(outcome)`
  - `step_profile(profile)`
  - `step_ratio(denom)`

- More informative error for some failures of
  [`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md).
  ([\#209](https://github.com/tidymodels/recipes/issues/209))

- [`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md)
  and
  [`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md)
  now gives more informative warnings when `impute_with` data contains
  all NAs. ([\#1385](https://github.com/tidymodels/recipes/issues/1385))

- [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  [`step_spline_monotone()`](https://recipes.tidymodels.org/dev/reference/step_spline_monotone.md),
  [`step_spline_natural()`](https://recipes.tidymodels.org/dev/reference/step_spline_natural.md),
  and
  [`step_spline_nonnegative()`](https://recipes.tidymodels.org/dev/reference/step_spline_nonnegative.md)
  now gives informative errors when applied to zero variance predictors.
  ([\#1455](https://github.com/tidymodels/recipes/issues/1455))

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  has gained `contrasts` argument. This change soft deprecates the use
  of `getOption("contrasts")` with
  [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md).
  (##1349)

### Bug Fixes

- Fixed printing for
  [`step_geodist()`](https://recipes.tidymodels.org/dev/reference/step_geodist.md)
  when no variables are selected.
  ([\#1423](https://github.com/tidymodels/recipes/issues/1423))

- Fixed bug where
  [`extract_fit_time()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  would throw warning for when recipe didn’t have any steps.
  ([\#1475](https://github.com/tidymodels/recipes/issues/1475))

- [`step_interact()`](https://recipes.tidymodels.org/dev/reference/step_interact.md)
  now works with empty selections instead of erroring.
  ([\#1417](https://github.com/tidymodels/recipes/issues/1417))

- fixed bug where
  [`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md)
  required that the Matrix package was loaded.
  ([\#1141](https://github.com/tidymodels/recipes/issues/1141))

- Fixed bug where
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  would error on sf objects.
  ([\#1393](https://github.com/tidymodels/recipes/issues/1393))

- [`step_cut()`](https://recipes.tidymodels.org/dev/reference/step_cut.md)
  not longer errors on NA values in
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).
  ([\#1304](https://github.com/tidymodels/recipes/issues/1304))

- Fixed bug in
  [`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md)
  would error on character vectors when `strings_as_factors = TRUE`.
  ([\#926](https://github.com/tidymodels/recipes/issues/926))

- Make it so
  [`recipe.formula()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  can’t take table objects as input, in accordance with documentation.
  ([\#1416](https://github.com/tidymodels/recipes/issues/1416))

- Fixed bug where
  [`step_lincomb()`](https://recipes.tidymodels.org/dev/reference/step_lincomb.md)
  would remove both variables if they were identical.
  ([\#1357](https://github.com/tidymodels/recipes/issues/1357))

- Fixed bugs in
  [`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md),
  [`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md),
  [`step_harmonic()`](https://recipes.tidymodels.org/dev/reference/step_harmonic.md),
  [`step_invlogit()`](https://recipes.tidymodels.org/dev/reference/step_invlogit.md),
  [`step_isomap()`](https://recipes.tidymodels.org/dev/reference/step_isomap.md),
  [`step_logit()`](https://recipes.tidymodels.org/dev/reference/step_logit.md),
  [`check_range()`](https://recipes.tidymodels.org/dev/reference/check_range.md),
  [`step_poly_bernstein()`](https://recipes.tidymodels.org/dev/reference/step_poly_bernstein.md),
  [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  `step_monotone()`, `step_natural()`, `step_nonnegative()` would error
  in [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
  with zero-row data.
  ([\#1219](https://github.com/tidymodels/recipes/issues/1219))

- fixed bug where `bake.step_discretize()` would error if applied to
  predictor only containing `NA`s.
  ([\#1350](https://github.com/tidymodels/recipes/issues/1350))

### Developer

- Added developer function
  [`check_options()`](https://recipes.tidymodels.org/dev/reference/check_options.md).
  ([\#1269](https://github.com/tidymodels/recipes/issues/1269))

- Officially deprecate
  [`printer()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md)
  in favor of
  [`print_step()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md).
  ([\#1243](https://github.com/tidymodels/recipes/issues/1243))

## recipes 1.2.1

CRAN release: 2025-03-25

### Bug Fixes

- Fixed bug where sparsity creation steps error if applied to variables
  created by other steps.
  ([\#1448](https://github.com/tidymodels/recipes/issues/1448))

## recipes 1.2.0

CRAN release: 2025-03-17

### Improvements

- [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md),
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), and
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) now
  work with sparse tibbles.
  ([\#1364](https://github.com/tidymodels/recipes/issues/1364),
  [\#1366](https://github.com/tidymodels/recipes/issues/1366))

- [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md),
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), and
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) now
  work with sparse matrices.
  ([\#1364](https://github.com/tidymodels/recipes/issues/1364),
  [\#1368](https://github.com/tidymodels/recipes/issues/1368),
  [\#1369](https://github.com/tidymodels/recipes/issues/1369))

- The following steps has gained the argument `sparse`. When set to
  `"yes"`, they will produce sparse vectors.
  ([\#1392](https://github.com/tidymodels/recipes/issues/1392))

  - [`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md)
  - [`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md)
  - [`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md)
  - [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  - [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  - [`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md)
  - [`step_indicate_na()`](https://recipes.tidymodels.org/dev/reference/step_indicate_na.md)
  - [`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md)

- The following steps have been modified to preserve sparsity in its
  input. ([\#1395](https://github.com/tidymodels/recipes/issues/1395))

  - [`step_arrange()`](https://recipes.tidymodels.org/dev/reference/step_arrange.md)
  - [`step_filter_missing()`](https://recipes.tidymodels.org/dev/reference/step_filter_missing.md)
  - [`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md)
  - [`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md)
  - [`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md)
  - [`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md)
  - [`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md)
  - [`step_rename_at()`](https://recipes.tidymodels.org/dev/reference/step_rename_at.md)
  - [`step_rename()`](https://recipes.tidymodels.org/dev/reference/step_rename.md)
  - [`step_rm()`](https://recipes.tidymodels.org/dev/reference/step_rm.md)
  - [`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md)
  - [`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md)
  - [`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md)
  - [`step_shuffle()`](https://recipes.tidymodels.org/dev/reference/step_shuffle.md)
  - [`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md)
  - [`step_sqrt()`](https://recipes.tidymodels.org/dev/reference/step_sqrt.md)
  - [`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)

- All steps and checks now require arguments `trained`, `skip`, `role`,
  and `id` at all times.
  ([\#1387](https://github.com/tidymodels/recipes/issues/1387))

### Bug Fixes

- Fixed bug where name repaired column names would get changed when
  baked for some steps.
  ([\#1347](https://github.com/tidymodels/recipes/issues/1347))

## recipes 1.1.1

CRAN release: 2025-02-12

### Improvements

- Example for
  [`step_novel()`](https://recipes.tidymodels.org/dev/reference/step_novel.md)
  now better illustrates how it works.
  ([@Edgar-Zamora](https://github.com/Edgar-Zamora),
  [\#1248](https://github.com/tidymodels/recipes/issues/1248))

- `prep.recipe(..., strings_as_factors = TRUE)` now only converts string
  variables that have role “predictor” or “outcome”.
  ([@dajmcdon](https://github.com/dajmcdon),
  [\#1358](https://github.com/tidymodels/recipes/issues/1358),
  [\#1376](https://github.com/tidymodels/recipes/issues/1376))

## recipes 1.1.0

CRAN release: 2024-07-04

### Improvements

- Improved error message for misspelled argument in step functions.
  ([\#1318](https://github.com/tidymodels/recipes/issues/1318))

- [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  can now take data.frames with list-columns or sf data.frames as input
  to `data`.
  ([\#1283](https://github.com/tidymodels/recipes/issues/1283))

- [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  will now show better error when columns are misspelled in formula
  ([\#1283](https://github.com/tidymodels/recipes/issues/1283)).

- [`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
  now errors if a column would simultaneously have roles `"outcome"` and
  `"predictor"`.
  ([\#935](https://github.com/tidymodels/recipes/issues/935))

- [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) will
  now error if the ptype of the data doesn’t match which was used to
  define the recipe.
  ([\#793](https://github.com/tidymodels/recipes/issues/793))

- Added more documentation in
  [`?selections`](https://recipes.tidymodels.org/dev/reference/selections.md)
  about how
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  works in recipes.
  ([\#1259](https://github.com/tidymodels/recipes/issues/1259))

- New
  [`extract_fit_time()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  method has been added that returns the time it took to train the
  recipe. ([\#1071](https://github.com/tidymodels/recipes/issues/1071))

- [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  [`step_spline_monotone()`](https://recipes.tidymodels.org/dev/reference/step_spline_monotone.md),
  and
  [`step_spline_nonnegative()`](https://recipes.tidymodels.org/dev/reference/step_spline_nonnegative.md)
  now throws informative errors if the`degree`, `deg_free`, and
  `complete_set` arguments causes an error.
  ([\#1170](https://github.com/tidymodels/recipes/issues/1170))

- [`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md)
  gained `.pkgs` argument to specify what packages need to be loaded for
  step to work.
  ([\#1282](https://github.com/tidymodels/recipes/issues/1282))

- [`step_interact()`](https://recipes.tidymodels.org/dev/reference/step_interact.md)
  now gives better error if `terms` isn’t a formula.
  ([\#1299](https://github.com/tidymodels/recipes/issues/1299))

- The `prefix` argument of
  [`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md)
  is now properly documented.
  ([\#1298](https://github.com/tidymodels/recipes/issues/1298))

- Significant speedup in
  [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  when applied to many columns.
  ([\#1305](https://github.com/tidymodels/recipes/issues/1305))

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  now gives an informative error on attempt to generate too many columns
  to fit in memory.
  ([\#828](https://github.com/tidymodels/recipes/issues/828))

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  and
  [`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md)
  now throw more informative warnings for unseen levels.
  ([\#450](https://github.com/tidymodels/recipes/issues/450))

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  now throws more informative warnings for `NA` values.
  ([\#450](https://github.com/tidymodels/recipes/issues/450))

- [`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md)
  now accepts `"mday"` as a possible feature.
  ([@Edgar-Zamora](https://github.com/Edgar-Zamora),
  [\#1211](https://github.com/tidymodels/recipes/issues/1211))

### Bug Fixes

- `NA` levels in factors aren’t dropped when passed to
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md).
  ([\#1291](https://github.com/tidymodels/recipes/issues/1291))

- [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  no longer crashes when given long formula expression
  ([\#1283](https://github.com/tidymodels/recipes/issues/1283)).

- Fixed bug in
  [`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md)
  and
  [`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md)
  where `knots` field in `options` argument wasn’t correctly used.
  ([\#1297](https://github.com/tidymodels/recipes/issues/1297))

- Bug fixed in
  [`step_interact()`](https://recipes.tidymodels.org/dev/reference/step_interact.md)
  where long formulas were used.
  ([\#1231](https://github.com/tidymodels/recipes/issues/1231),
  [\#1289](https://github.com/tidymodels/recipes/issues/1289))

- Fixed documentation mistake where default value of
  `keep_original_cols` argument were wrong.
  ([\#1314](https://github.com/tidymodels/recipes/issues/1314))

### Developer

- Developer helper function
  [`recipes_ptype()`](https://recipes.tidymodels.org/dev/reference/recipes_ptype.md)
  has been added, returning expected input data for
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) for a
  given recipe object.
  ([\#1329](https://github.com/tidymodels/recipes/issues/1329))

- Developer helper function
  [`recipes_ptype_validate()`](https://recipes.tidymodels.org/dev/reference/recipes_ptype_validate.md)
  has been added, to validate new data is compatible with recipe ptype.
  ([\#793](https://github.com/tidymodels/recipes/issues/793))

- Developer helper functions
  [`recipes_names_predictors()`](https://recipes.tidymodels.org/dev/reference/recipes-role-indicator.md)
  and
  [`recipes_names_outcomes()`](https://recipes.tidymodels.org/dev/reference/recipes-role-indicator.md)
  have been added to aid variable selection in steps.
  ([\#1026](https://github.com/tidymodels/recipes/issues/1026))

## recipes 1.0.10

CRAN release: 2024-02-18

### Bug Fixes

- Fixed bug where
  [`step_log()`](https://recipes.tidymodels.org/dev/reference/step_log.md)
  breaks legacy recipe objects by indexing `names(object)` in
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).
  ([@stufield](https://github.com/stufield),
  [\#1284](https://github.com/tidymodels/recipes/issues/1284))

## recipes 1.0.9

CRAN release: 2023-12-13

### Improvements

- Minor speed-up and reduced memory consumption for
  [`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md)
  in the
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) stage
  by reducing unused multiplications
  ([@jkennel](https://github.com/jkennel),
  [\#1265](https://github.com/tidymodels/recipes/issues/1265))

- Document that
  [`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md),
  [`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
  and
  [`remove_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
  are applied before steps and checks.
  ([\#778](https://github.com/tidymodels/recipes/issues/778))

- Documentation for tidy methods for all steps has been added when
  missing and improved to describe the return value more accurately.
  ([\#936](https://github.com/tidymodels/recipes/issues/936))

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  will now error if passed character instead of loudly ignoring them.
  Only applicable when setting `strings_as_factors = FALSE`.
  ([\#1233](https://github.com/tidymodels/recipes/issues/1233))

- It is now documented that
  [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md)
  can be made periodic.
  ([\#1223](https://github.com/tidymodels/recipes/issues/1223))

- [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) now
  correctly throws a warning when `training` argument is set when
  prepping a prepped recipe, telling the user that it will be ignored.
  ([\#1244](https://github.com/tidymodels/recipes/issues/1244))

- When errors are thrown about wrongly typed input to steps, the
  offending variables and their types are now listed.
  ([\#1217](https://github.com/tidymodels/recipes/issues/1217))

- All warnings and errors have been updated to use the cli package for
  increased clarity and consistency.
  ([\#1237](https://github.com/tidymodels/recipes/issues/1237))

- Added warnings when
  [`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md),
  `step_normalise()`,
  [`step_center()`](https://recipes.tidymodels.org/dev/reference/step_center.md)
  or
  [`step_range()`](https://recipes.tidymodels.org/dev/reference/step_range.md)
  result in `NaN` columns. ([@mastoffel](https://github.com/mastoffel),
  [\#1221](https://github.com/tidymodels/recipes/issues/1221))

### Bug Fixes

- Fixed bug where
  [`step_factor2string()`](https://recipes.tidymodels.org/dev/reference/step_factor2string.md)
  if `strings_as_factors = TRUE` is set in
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md).
  ([\#317](https://github.com/tidymodels/recipes/issues/317))

- Fixed bug where
  [`tidy.step_cut()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
  always returned zero row tibbles for trained recipes.
  ([\#1229](https://github.com/tidymodels/recipes/issues/1229))

## recipes 1.0.8

CRAN release: 2023-08-25

### Improvements

- Minor speed-up and reduced memory consumption for spline steps that
  rely on `spline2_apply`
  ([\#1200](https://github.com/tidymodels/recipes/issues/1200))

### Bug Fixes

- Fixed bugs where spline steps
  ([`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md),
  [`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md),
  [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  [`step_spline_monotone()`](https://recipes.tidymodels.org/dev/reference/step_spline_monotone.md),
  [`step_spline_natural()`](https://recipes.tidymodels.org/dev/reference/step_spline_natural.md),
  [`step_spline_nonnegative()`](https://recipes.tidymodels.org/dev/reference/step_spline_nonnegative.md))
  would error if baked with 1 row.
  ([\#1191](https://github.com/tidymodels/recipes/issues/1191))

## recipes 1.0.7

CRAN release: 2023-08-10

### New Steps

- [`step_classdist_shrunken()`](https://recipes.tidymodels.org/dev/reference/step_classdist_shrunken.md),
  a regularized version of
  [`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
  was added.
  ([\#1185](https://github.com/tidymodels/recipes/issues/1185))

### Improvements

- [`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md)
  and
  [`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md)
  have gained `keep_original_cols` argument.
  ([\#1164](https://github.com/tidymodels/recipes/issues/1164))

- The `keep_original_cols` argument has been added to
  [`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
  [`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
  [`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md),
  [`step_geodist()`](https://recipes.tidymodels.org/dev/reference/step_geodist.md),
  [`step_indicate_na()`](https://recipes.tidymodels.org/dev/reference/step_indicate_na.md),
  [`step_interact()`](https://recipes.tidymodels.org/dev/reference/step_interact.md),
  [`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md),
  [`step_poly()`](https://recipes.tidymodels.org/dev/reference/step_poly.md),
  [`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md),
  [`step_window()`](https://recipes.tidymodels.org/dev/reference/step_window.md).
  The default for each step is set to preserve past behavior. This
  change should mean that every step that produces new columns has the
  `keep_original_cols` argument.
  ([\#1167](https://github.com/tidymodels/recipes/issues/1167))

### Bug Fixes

- Fixed bugs where
  [`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
  [`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md),
  [`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md),
  [`step_geodist()`](https://recipes.tidymodels.org/dev/reference/step_geodist.md),
  [`step_interact()`](https://recipes.tidymodels.org/dev/reference/step_interact.md),
  [`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md),
  and
  [`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md)
  didn’t work with empty selection. All steps now leave data unmodified
  when having empty selections.
  ([\#1142](https://github.com/tidymodels/recipes/issues/1142))

- [`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md),
  [`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md)
  and
  [`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md)
  no longer returns a column with all `NA`s with empty selections.
  ([\#1142](https://github.com/tidymodels/recipes/issues/1142))

- [`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md)
  no longer returns a column with all 0s with empty selections.
  ([\#1142](https://github.com/tidymodels/recipes/issues/1142))

- The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) methods
  for
  [`step_geodist()`](https://recipes.tidymodels.org/dev/reference/step_geodist.md),
  [`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md),
  and
  [`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md)
  now correctly return zero-row tibbles when used with empty selections.
  ([\#1144](https://github.com/tidymodels/recipes/issues/1144))

- [`step_poly_bernstein()`](https://recipes.tidymodels.org/dev/reference/step_poly_bernstein.md),
  [`step_profile()`](https://recipes.tidymodels.org/dev/reference/step_profile.md),
  [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  [`step_spline_monotone()`](https://recipes.tidymodels.org/dev/reference/step_spline_monotone.md),
  [`step_spline_natural()`](https://recipes.tidymodels.org/dev/reference/step_spline_natural.md),
  and
  [`step_spline_nonnegative()`](https://recipes.tidymodels.org/dev/reference/step_spline_nonnegative.md)
  now correctly return a zero row tibble when used with empty selection.
  ([\#1133](https://github.com/tidymodels/recipes/issues/1133))

- Fixed bug where the
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method for
  [`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md)
  didn’t return an `id` column.
  ([\#1144](https://github.com/tidymodels/recipes/issues/1144))

- [`check_class()`](https://recipes.tidymodels.org/dev/reference/check_class.md),
  [`check_missing()`](https://recipes.tidymodels.org/dev/reference/check_missing.md),
  [`check_new_values()`](https://recipes.tidymodels.org/dev/reference/check_new_values.md),
  [`check_range()`](https://recipes.tidymodels.org/dev/reference/check_range.md),
  [`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md),
  [`step_poly_bernstein()`](https://recipes.tidymodels.org/dev/reference/step_poly_bernstein.md),
  [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  [`step_spline_monotone()`](https://recipes.tidymodels.org/dev/reference/step_spline_monotone.md),
  [`step_spline_natural()`](https://recipes.tidymodels.org/dev/reference/step_spline_natural.md),
  [`step_spline_nonnegative()`](https://recipes.tidymodels.org/dev/reference/step_spline_nonnegative.md),
  and
  [`step_string2factor()`](https://recipes.tidymodels.org/dev/reference/step_string2factor.md)
  now throw an informative error if needed non-standard role columns are
  missing during
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).
  ([\#1145](https://github.com/tidymodels/recipes/issues/1145))

### Breaking Changes

- [`step_window()`](https://recipes.tidymodels.org/dev/reference/step_window.md)
  now throws an error instead of silently overwriting if `names`
  argument overlaps with existing columns.
  ([\#1172](https://github.com/tidymodels/recipes/issues/1172))

- [`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md)
  and
  [`step_count()`](https://recipes.tidymodels.org/dev/reference/step_count.md)
  will now informatively error if name collision occurs.
  ([\#1169](https://github.com/tidymodels/recipes/issues/1169))

### Developer

- Added developer function
  [`remove_original_cols()`](https://recipes.tidymodels.org/dev/reference/remove_original_cols.md)
  to help remove original columns that are no longer needed.
  ([\#1149](https://github.com/tidymodels/recipes/issues/1149))

- Added developer function
  [`recipes_remove_cols()`](https://recipes.tidymodels.org/dev/reference/recipes_remove_cols.md)
  to provide standardized way to remove columns by column names.
  ([\#1155](https://github.com/tidymodels/recipes/issues/1155))

## recipes 1.0.6

CRAN release: 2023-04-25

### Improvements

- Steps with tunable arguments now have those arguments listed in the
  documentation.

- All steps that add new columns will now informatively error if name
  collision occurs.
  ([\#983](https://github.com/tidymodels/recipes/issues/983))

### Bug Fixes

- Fixed bug in
  [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  [`step_spline_monotone()`](https://recipes.tidymodels.org/dev/reference/step_spline_monotone.md),
  and `spline_nonnegative()` where you weren’t able to tune the `degree`
  argument.

- [`step_range()`](https://recipes.tidymodels.org/dev/reference/step_range.md)
  now perform correctly performs clipping on recipes created before
  1.0.3. ([\#1097](https://github.com/tidymodels/recipes/issues/1097))

### Breaking Changes

- The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method
  for
  [`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md),
  [`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md),
  and
  [`step_impute_mode()`](https://recipes.tidymodels.org/dev/reference/step_impute_mode.md)
  now the imputed value with the column name `value` instead of `model`.
  This is in line with the output of
  [`step_impute_lower()`](https://recipes.tidymodels.org/dev/reference/step_impute_lower.md).
  ([\#826](https://github.com/tidymodels/recipes/issues/826))

## recipes 1.0.5

CRAN release: 2023-02-20

- Added `outside` argument to
  [`step_percentile()`](https://recipes.tidymodels.org/dev/reference/step_percentile.md)
  to determine different ways of handling values outside the range of
  the training data.

- [`step_range()`](https://recipes.tidymodels.org/dev/reference/step_range.md)
  is now backwards compatible with respect to the `clipping` argument
  that was added 1.0.3, and old saved recipes can now be baked.
  ([\#1090](https://github.com/tidymodels/recipes/issues/1090))

- update print methods to use cli package for formatting.
  ([\#426](https://github.com/tidymodels/recipes/issues/426))

- Print methods no longer errors for untrained recipes with long
  selections.
  ([\#1083](https://github.com/tidymodels/recipes/issues/1083))

- The `recipe`, `step`, and `check` methods for
  [`generics::tune_args()`](https://generics.r-lib.org/reference/tune_args.html)
  are now registered unconditionally (tidymodels/workflows#192).

- Added a [`conditionMessage()`](https://rdrr.io/r/base/conditions.html)
  method for `recipes_error`s to consistently point out which step
  errors occurred in when reporting errors.
  ([\#1080](https://github.com/tidymodels/recipes/issues/1080))

## recipes 1.0.4

CRAN release: 2023-01-11

- Added missing tidy method for
  [`step_intercept()`](https://recipes.tidymodels.org/dev/reference/step_intercept.md)
  and
  [`step_lag()`](https://recipes.tidymodels.org/dev/reference/step_lag.md).
  ([\#730](https://github.com/tidymodels/recipes/issues/730))

- Errors in
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) will
  now indicate which step caused the error.
  ([\#420](https://github.com/tidymodels/recipes/issues/420))

- Developer focused
  [`check_type()`](https://recipes.tidymodels.org/dev/reference/check_type.md)
  got a new `types` argument for more precise checking of column types.

- [`recipes_extension_check()`](https://recipes.tidymodels.org/dev/reference/recipes_extension_check.md)
  have been added. This developer focused function checks that steps
  have all the required S3 methods.

- [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  now error more informatively when `data` is missing.
  ([\#1042](https://github.com/tidymodels/recipes/issues/1042))

## recipes 1.0.3

CRAN release: 2022-11-09

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  no longer returns integer columns as there are a number of contrast
  methods that return fractional values.
  ([\#1053](https://github.com/tidymodels/recipes/issues/1053))

- Fixed a 0-length recycling bug in
  [`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md)
  exposed by the development version of purrr
  ([\#1052](https://github.com/tidymodels/recipes/issues/1052)).

- Types of variables have been made granular. `"nominal"` has been split
  into `"ordered"` and `"unordered"` and `"numeric"` has been split into
  `"double"` and `"integer"`.
  ([\#993](https://github.com/tidymodels/recipes/issues/993))

- New selectors:
  [`all_double()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  [`all_ordered()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  [`all_unordered()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  [`all_date()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
  and
  [`all_datetime()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
  in addition to the existing
  [`all_numeric()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
  and
  [`all_nominal()`](https://recipes.tidymodels.org/dev/reference/has_role.md).
  All selectors come with a `*_predictors()` variant.
  ([\#993](https://github.com/tidymodels/recipes/issues/993))

- Developer focused
  [`.get_data_types()`](https://recipes.tidymodels.org/dev/reference/get_data_types.md)
  generic has been added to designate types of columns. Exported for use
  in extension packages that deal with types not supported in recipes
  directly. ([\#993](https://github.com/tidymodels/recipes/issues/993))

- The
  [`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md)
  function now defaults to using the clock package to format day-of-week
  and month labels.
  ([\#1048](https://github.com/tidymodels/recipes/issues/1048))

- [`step_range()`](https://recipes.tidymodels.org/dev/reference/step_range.md)
  has gained a argument `clipping` that when set to `FALSE` no longer
  clips the data to be between `min` and `max`.

- Added documentation regarding developer functions
  [`?developer_functions`](https://recipes.tidymodels.org/dev/reference/developer_functions.md).
  ([\#1163](https://github.com/tidymodels/recipes/issues/1163))

## recipes 1.0.2

CRAN release: 2022-10-15

- A new set of basis functions were added:
  [`step_spline_b()`](https://recipes.tidymodels.org/dev/reference/step_spline_b.md),
  [`step_spline_convex()`](https://recipes.tidymodels.org/dev/reference/step_spline_convex.md),
  [`step_spline_monotone()`](https://recipes.tidymodels.org/dev/reference/step_spline_monotone.md),
  [`step_spline_natural()`](https://recipes.tidymodels.org/dev/reference/step_spline_natural.md),
  [`step_spline_nonnegative()`](https://recipes.tidymodels.org/dev/reference/step_spline_nonnegative.md),
  and
  [`step_poly_bernstein()`](https://recipes.tidymodels.org/dev/reference/step_poly_bernstein.md).

- [`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md),
  [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md),
  [`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md),
  [`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md),
  [`step_ordinalscore()`](https://recipes.tidymodels.org/dev/reference/step_ordinalscore.md),
  and
  [`step_regex()`](https://recipes.tidymodels.org/dev/reference/step_regex.md)
  now returns integer results when appropriate.
  ([\#766](https://github.com/tidymodels/recipes/issues/766))

- The default for the `strict` argument in
  [`step_integer()`](https://recipes.tidymodels.org/dev/reference/step_integer.md)
  has been changed from `FALSE` to `TRUE`. The function will thus return
  integers, rather than whole-number numerics, by default.
  ([\#766](https://github.com/tidymodels/recipes/issues/766))

- The default for the `value` argument in
  [`step_intercept()`](https://recipes.tidymodels.org/dev/reference/step_intercept.md)
  has been changed from `1` to `1L`.
  ([\#766](https://github.com/tidymodels/recipes/issues/766))

## recipes 1.0.1

CRAN release: 2022-07-07

- Fixed bug where
  [`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md)
  didn’t work if it isn’t have any missing values.
  ([\#1019](https://github.com/tidymodels/recipes/issues/1019))

## recipes 1.0.0

CRAN release: 2022-07-01

### Improvements and Other Changes

- Added support for case weights in the following steps

  - [`step_center()`](https://recipes.tidymodels.org/dev/reference/step_center.md)
  - [`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md)
  - [`step_corr()`](https://recipes.tidymodels.org/dev/reference/step_corr.md)
  - [`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md)
  - [`step_filter_missing()`](https://recipes.tidymodels.org/dev/reference/step_filter_missing.md)
  - [`step_impute_linear()`](https://recipes.tidymodels.org/dev/reference/step_impute_linear.md)
  - [`step_impute_mean()`](https://recipes.tidymodels.org/dev/reference/step_impute_mean.md)
  - [`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md)
  - [`step_impute_mode()`](https://recipes.tidymodels.org/dev/reference/step_impute_mode.md)
  - [`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md)
  - [`step_nzv()`](https://recipes.tidymodels.org/dev/reference/step_nzv.md)
  - [`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md)
  - [`step_percentile()`](https://recipes.tidymodels.org/dev/reference/step_percentile.md)
  - [`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md)
  - [`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md)
  - [`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md)

- A number of developer focused functions to deal with case weights are
  added:
  [`are_weights_used()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md),
  [`get_case_weights()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md),
  [`averages()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md),
  [`medians()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md),
  [`variances()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md),
  [`correlations()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md),
  [`covariances()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md),
  and
  [`pca_wts()`](https://recipes.tidymodels.org/dev/reference/case-weight-helpers.md)

- recipes now checks that all columns in the `data` supplied to
  [`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
  are also present in the `new_data` supplied to
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md). An
  exception is made for columns with roles of either `"outcome"` or
  `"case_weights"`, which are typically not required at
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) time.
  The new
  [`update_role_requirements()`](https://recipes.tidymodels.org/dev/reference/update_role_requirements.md)
  function can be used to adjust whether or not columns of a particular
  role are required at
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) time
  if you need to opt out of this check
  ([\#1011](https://github.com/tidymodels/recipes/issues/1011)).

- The [`summary()`](https://rdrr.io/r/base/summary.html) method for
  recipe objects now contains an extra column to indicate which columns
  are required when
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) is
  used.

### New Steps

- [`step_time()`](https://recipes.tidymodels.org/dev/reference/step_time.md)
  has been added that extracts time features such as hour, minute, or
  second. ([\#968](https://github.com/tidymodels/recipes/issues/968))

### Bug Fixes

- Fixed bug in which functions that
  [`step_hyperbolic()`](https://recipes.tidymodels.org/dev/reference/step_hyperbolic.md)
  uses ([\#932](https://github.com/tidymodels/recipes/issues/932)).

- [`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md)
  now respects factor-levels of the selected variables when creating
  dummies. ([\#916](https://github.com/tidymodels/recipes/issues/916))

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  no works correctly with recipes trained on version 0.1.17 or earlier.
  ([\#921](https://github.com/tidymodels/recipes/issues/921))

- Fixed a bug where setting `fresh = TRUE` in
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
  wouldn’t result in re-prepping the recipe.
  ([\#492](https://github.com/tidymodels/recipes/issues/492))

- Bug was fixed in
  [`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md)
  which used to error when it was applied to variable with missing
  values. ([\#743](https://github.com/tidymodels/recipes/issues/743))

- A bug was fixed in
  [`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md)
  which used to error if 1 variable was selected.
  ([\#963](https://github.com/tidymodels/recipes/issues/963))

### Improvements and Other Changes

- Finally removed `step_upsample()` and `step_downsample()` in recipes
  as they are now available in the themis package.

- [`discretize()`](https://recipes.tidymodels.org/dev/reference/discretize.md)
  and
  [`step_discretize()`](https://recipes.tidymodels.org/dev/reference/step_discretize.md)
  now can return factor levels similar to
  [`cut()`](https://rdrr.io/r/base/cut.html).
  ([\#674](https://github.com/tidymodels/recipes/issues/674))

- [`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md)
  now actually had their defaults for `skip` changed to `TRUE` as was
  stated in release 0.1.13. (934)

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  has been made more robust to non-standard column names.
  ([\#879](https://github.com/tidymodels/recipes/issues/879))

- [`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md)
  now allows you use use multiple outcomes if they are numeric.
  ([\#651](https://github.com/tidymodels/recipes/issues/651))

- [`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md)
  and
  [`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md)
  ignore columns with zero variance, generate a warning and suggest to
  use
  [`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)
  ([\#920](https://github.com/tidymodels/recipes/issues/920)).

- printing for
  [`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md)
  now show variables that were imputed instead of variables used for
  imputing. ([\#837](https://github.com/tidymodels/recipes/issues/837))

- [`step_discretize()`](https://recipes.tidymodels.org/dev/reference/step_discretize.md)
  and
  [`discretize()`](https://recipes.tidymodels.org/dev/reference/discretize.md)
  will automatically remove missing values if `keep_na = TRUE`, removing
  the need to specify `keep_na = TRUE` and `na.rm = TRUE`.
  ([\#982](https://github.com/tidymodels/recipes/issues/982))

- [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) and
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
  checks and errors if output of `bake.bake_*()` isn’t a tibble.

- [`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md)
  now has a locale argument that can be used to control how the `month`
  and `dow` features are returned.
  ([\#1000](https://github.com/tidymodels/recipes/issues/1000))

## recipes 0.2.0

CRAN release: 2022-02-18

### New Steps

- [`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md)
  uses a different implementation of non-negative matrix factorization
  that is much faster and enables regularized estimation.
  ([\#790](https://github.com/tidymodels/recipes/issues/790))

- [`step_dummy_extract()`](https://recipes.tidymodels.org/dev/reference/step_dummy_extract.md)
  creates multiple variables from a character variable by extracting
  elements using regular expressions and counting those elements.

- [`step_filter_missing()`](https://recipes.tidymodels.org/dev/reference/step_filter_missing.md)
  can filter columns based on proportion of missingness
  ([\#270](https://github.com/tidymodels/recipes/issues/270)).

- [`step_percentile()`](https://recipes.tidymodels.org/dev/reference/step_percentile.md)
  replaces the value of a variable with its percentile from the training
  set. ([\#765](https://github.com/tidymodels/recipes/issues/765))

### Improvements and Other Changes

- All recipe steps now officially support empty selections to be more
  aligned with dplyr and other packages that use tidyselect
  ([\#603](https://github.com/tidymodels/recipes/issues/603),
  [\#531](https://github.com/tidymodels/recipes/issues/531)). For
  example, if a previous step removed all of the columns need for a
  later step, the recipe does not fail when it is estimated (with the
  exception of
  [`step_mutate()`](https://recipes.tidymodels.org/dev/reference/step_mutate.md)).
  The documentation in
  [`?selections`](https://recipes.tidymodels.org/dev/reference/selections.md)
  has been updated with advice for writing selectors when filtering
  steps are used.
  ([\#813](https://github.com/tidymodels/recipes/issues/813))

- Fixed bug in
  [`step_harmonic()`](https://recipes.tidymodels.org/dev/reference/step_harmonic.md)
  printing and changed defaults to `role = "predictor"` and
  `keep_original_cols = FALSE`
  ([\#822](https://github.com/tidymodels/recipes/issues/822)).

- Improved the efficiency of computations for the Box-Cox transformation
  ([\#820](https://github.com/tidymodels/recipes/issues/820)).

- When a feature extraction step (e.g.,
  [`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
  [`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md),
  etc.) has zero components specified, the
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method now
  lists the selected columns in the `terms` column.

- Deprecation has started for
  [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md)
  in favor of
  [`step_nnmf_sparse()`](https://recipes.tidymodels.org/dev/reference/step_nnmf_sparse.md).
  ([\#790](https://github.com/tidymodels/recipes/issues/790))

- Steps now have a dedicated subsection detailing what happens when
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) is applied.
  ([\#876](https://github.com/tidymodels/recipes/issues/876))

- [`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md)
  now runs `fastICA()` using a specific set of random numbers so that
  initialization is reproducible.

- [`tidy.recipe()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
  now returns a zero row tibble instead of an error when applied to a
  empty recipe.
  ([\#867](https://github.com/tidymodels/recipes/issues/867))

- [`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)
  now has a `group` argument. The same filter is applied but looks for
  zero-variance within 1 or more columns that define groups.
  ([\#711](https://github.com/tidymodels/recipes/issues/711))

- [`detect_step()`](https://recipes.tidymodels.org/dev/reference/detect_step.md)
  is no longer restricted to steps created in recipes
  ([\#869](https://github.com/tidymodels/recipes/issues/869)).

- New
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  and
  [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  methods to extract parameter sets and single parameters from `recipe`
  objects.

- [`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md)
  now allow for setting `threshold = 0` which will result in no
  othering. ([\#904](https://github.com/tidymodels/recipes/issues/904))

### Breaking Changes

- [`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md)
  now indirectly uses the `fastICA` package since that package has
  increased their R version requirement. Recipe objects from previous
  versions will error when applied to new data.
  ([\#823](https://github.com/tidymodels/recipes/issues/823))

- `step_kpca*()` now directly use the `kernlab` package. Recipe objects
  from previous versions will error when applied to new data.

- [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) will
  now error if `new_data` doesn’t contain all the required columns.
  ([\#491](https://github.com/tidymodels/recipes/issues/491))

### Developer

- The print methods have been internally changes to use
  [`print_step()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md)
  instead of
  [`printer()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md).
  This is done for a smoother transition to use `cli` in the next
  version. ([\#871](https://github.com/tidymodels/recipes/issues/871))

## recipes 0.1.17

CRAN release: 2021-09-27

### New Steps

- Added new
  [`step_harmonic()`](https://recipes.tidymodels.org/dev/reference/step_harmonic.md)
  ([\#702](https://github.com/tidymodels/recipes/issues/702)).

- Added a new step called
  [`step_dummy_multi_choice()`](https://recipes.tidymodels.org/dev/reference/step_dummy_multi_choice.md),
  which will take multiple nominal variables and produces shared dummy
  variables. ([\#716](https://github.com/tidymodels/recipes/issues/716))

### Deprecation News

- The deprecation for `step_upsample()` and `step_downsample()` has been
  escalated from a deprecation warning to a deprecation error; these
  functions are available in the themis package.

- Escalate deprecation for old versions of imputation steps (such as
  [`step_bagimpute()`](https://recipes.tidymodels.org/dev/reference/step_bagimpute.md))
  from a soft deprecation to a regular deprecation; these imputation
  steps have new names like
  [`step_impute_bag()`](https://recipes.tidymodels.org/dev/reference/step_impute_bag.md)
  ([\#753](https://github.com/tidymodels/recipes/issues/753)).

- [`step_kpca()`](https://recipes.tidymodels.org/dev/reference/step_kpca.md)
  was un-deprecated and gained the `keep_original_cols` argument.

- The deprecation of the `preserve` argument to
  [`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md)
  and
  [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  was escalated from a soft deprecation to regular deprecation.

- The deprecation of the `options` argument to
  [`step_nzv()`](https://recipes.tidymodels.org/dev/reference/step_nzv.md)
  was escalated to a deprecation error.

### Bug Fixes

- Fix imputation steps for new data that is all `NA`, and generate a
  warning for recipes created under previous versions that cannot be
  imputed with this fix
  ([\#719](https://github.com/tidymodels/recipes/issues/719)).

- A bug was fixed where imputed values via bagged trees would have the
  wrong levels.

### Improvements and Other Changes

- The computations for the Yeo-Johnson transformation were made more
  efficient ([\#782](https://github.com/tidymodels/recipes/issues/782)).

- New
  [`recipes_eval_select()`](https://recipes.tidymodels.org/dev/reference/recipes_eval_select.md)
  which is a developer tool that is useful for creating new recipes
  steps. It powers the tidyselect semantics that are specific to recipes
  and supports the modern tidyselect API introduced in tidyselect 1.0.0.
  Additionally, the older
  [`terms_select()`](https://recipes.tidymodels.org/dev/reference/terms_select.md)
  has been deprecated in favor of this new helper
  ([\#739](https://github.com/tidymodels/recipes/issues/739)).

- Speed-up/simplification to
  [`step_spatialsign()`](https://recipes.tidymodels.org/dev/reference/step_spatialsign.md)

- When only the terms attributes are desired from `model.frame` use the
  first row of data to improve speed and memory use
  ([\#726](https://github.com/tidymodels/recipes/issues/726)).

- Use Haversine formula for latitude-longitude pairs in
  [`step_geodist()`](https://recipes.tidymodels.org/dev/reference/step_geodist.md)
  ([\#725](https://github.com/tidymodels/recipes/issues/725)).

- Reorganize documentation for all recipe step `tidy` methods
  ([\#701](https://github.com/tidymodels/recipes/issues/701)).

- Generate warning when user attempts a Box-Cox transformation of
  non-positive data ([@LiamBlake](https://github.com/LiamBlake),
  [\#713](https://github.com/tidymodels/recipes/issues/713)).

- [`step_logit()`](https://recipes.tidymodels.org/dev/reference/step_logit.md)
  gained an offset argument for cases where the input is either zero or
  one ([\#784](https://github.com/tidymodels/recipes/issues/784))

- The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) methods
  for objects from
  [`check_new_values()`](https://recipes.tidymodels.org/dev/reference/check_new_values.md),
  [`check_class()`](https://recipes.tidymodels.org/dev/reference/check_class.md)
  and
  [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md)
  are now exported.

## recipes 0.1.16

CRAN release: 2021-04-16

### New Steps

- Added a new step called
  [`step_indicate_na()`](https://recipes.tidymodels.org/dev/reference/step_indicate_na.md),
  which will create and append additional binary columns to the data set
  to indicate which observations are missing
  ([\#623](https://github.com/tidymodels/recipes/issues/623)).

- Added new
  [`step_select()`](https://recipes.tidymodels.org/dev/reference/step_select.md)
  ([\#199](https://github.com/tidymodels/recipes/issues/199)).

### Bug Fixes

- The `threshold` argument of
  [`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md)
  is now
  [`tunable()`](https://generics.r-lib.org/reference/tunable.html)
  ([\#534](https://github.com/tidymodels/recipes/issues/534)).

- Integer variables used in
  [`step_profile()`](https://recipes.tidymodels.org/dev/reference/step_profile.md)
  are now kept as integers (and not doubles).

- Preserve multiple roles in `last_term_info` so
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) can
  correctly respond to `has_roles`.
  ([\#632](https://github.com/tidymodels/recipes/issues/632))

- Fixed behavior of the retain flag in
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
  ([\#652](https://github.com/tidymodels/recipes/issues/652)).

- The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) methods
  for
  [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md)
  was rewritten since it was not great
  ([\#665](https://github.com/tidymodels/recipes/issues/665)), and
  [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md)
  now no longer fully loads underlying packages
  ([\#685](https://github.com/tidymodels/recipes/issues/685)).

### Improvements and Other Changes

- Two new selectors that combine role and data type were added:
  [`all_numeric_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
  and
  [`all_nominal_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md).
  ([\#620](https://github.com/tidymodels/recipes/issues/620))

- Changed the names of all imputation steps, for example, from
  [`step_knnimpute()`](https://recipes.tidymodels.org/dev/reference/step_knnimpute.md)
  or
  [`step_medianimpute()`](https://recipes.tidymodels.org/dev/reference/step_medianimpute.md)
  (old) to
  [`step_impute_knn()`](https://recipes.tidymodels.org/dev/reference/step_impute_knn.md)
  or
  [`step_impute_median()`](https://recipes.tidymodels.org/dev/reference/step_impute_median.md)
  (new) ([\#614](https://github.com/tidymodels/recipes/issues/614)).

- Added `keep_original_cols` argument to several steps:

  - [`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
    [`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md),
    [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md),
    [`step_kpca_rbf()`](https://recipes.tidymodels.org/dev/reference/step_kpca_rbf.md),
    [`step_kpca_poly()`](https://recipes.tidymodels.org/dev/reference/step_kpca_poly.md),
    [`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md),
    [`step_isomap()`](https://recipes.tidymodels.org/dev/reference/step_isomap.md)
    which all default to `FALSE`
    ([\#635](https://github.com/tidymodels/recipes/issues/635)).
  - [`step_ratio()`](https://recipes.tidymodels.org/dev/reference/step_ratio.md),
    [`step_holiday()`](https://recipes.tidymodels.org/dev/reference/step_holiday.md),
    [`step_date()`](https://recipes.tidymodels.org/dev/reference/step_date.md)
    which all default to `TRUE` to maintain original behavior, as well
    as
    [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
    which defaults to `FALSE`
    ([\#645](https://github.com/tidymodels/recipes/issues/645)).

- Added `allow_rename` argument to
  [`recipes_eval_select()`](https://recipes.tidymodels.org/dev/reference/recipes_eval_select.md)
  ([\#646](https://github.com/tidymodels/recipes/issues/646)).

- Performance improvements for
  [`step_bs()`](https://recipes.tidymodels.org/dev/reference/step_bs.md)
  and
  [`step_ns()`](https://recipes.tidymodels.org/dev/reference/step_ns.md).
  The [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
  step no longer evaluates the basis functions on the training set and
  the [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)
  steps only evaluates the basis functions once for each unique input
  value ([\#574](https://github.com/tidymodels/recipes/issues/574))

- The `neighbors` parameter’s default range for
  [`step_isomap()`](https://recipes.tidymodels.org/dev/reference/step_isomap.md)
  was changed to be 20-80.

- The deprecation for `step_upsample()` and `step_downsample()` has been
  escalated from a soft deprecation to a regular deprecation; these
  functions are available in the themis package.

- Re-licensed package from GPL-2 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/recipes/issues/670).

## recipes 0.1.15

CRAN release: 2020-11-11

- The full tidyselect DSL is now allowed inside recipes `step_*()`
  functions. This includes the operators `&`, `|`, `-` and `!` and the
  new [`where()`](https://tidyselect.r-lib.org/reference/where.html)
  function. Additionally, the restriction preventing user defined
  selectors from being used has been lifted
  ([\#572](https://github.com/tidymodels/recipes/issues/572)).

- If steps that drop/add variables are skipped when baking the test set,
  the resulting column ordering of the baked test set will now be
  relative to the original recipe specification rather than relative to
  the baked training set. This is often more intuitive.

- More infrastructure work to make parallel processing on Windows less
  buggy with PSOCK clusters

- [`fully_trained()`](https://recipes.tidymodels.org/dev/reference/fully_trained.md)
  now returns `FALSE` when an unprepped recipe is used.

## recipes 0.1.14

CRAN release: 2020-10-17

- [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
  gained an option to print a summary of which columns were added and/or
  removed during execution.

- To reduce confusion between
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) and
  [`juice()`](https://recipes.tidymodels.org/dev/reference/juice.md),
  the latter is superseded in favor of using
  `bake(object, new_data = NULL)`. The `new_data` argument now has no
  default, so a `NULL` value must be explicitly used in order to emulate
  the results of
  [`juice()`](https://recipes.tidymodels.org/dev/reference/juice.md).
  [`juice()`](https://recipes.tidymodels.org/dev/reference/juice.md)
  will remain in the package (and used internally) but most
  communication and training will use `bake(object, new_data = NULL)`.
  ([\#543](https://github.com/tidymodels/recipes/issues/543))

- Tim Zhou added a step to use linear models for imputation
  ([\#555](https://github.com/tidymodels/recipes/issues/555))

## recipes 0.1.13

CRAN release: 2020-06-23

### Breaking Changes

- [`step_filter()`](https://recipes.tidymodels.org/dev/reference/step_filter.md),
  [`step_slice()`](https://recipes.tidymodels.org/dev/reference/step_slice.md),
  [`step_sample()`](https://recipes.tidymodels.org/dev/reference/step_sample.md),
  and
  [`step_naomit()`](https://recipes.tidymodels.org/dev/reference/step_naomit.md)
  had their defaults for `skip` changed to `TRUE`. In the vast majority
  of applications, these steps should not be applied to the test or
  assessment sets.

- `tidyr` version 1.0.0 or later is now required.

### Other Changes

- [`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md)
  was changed so that it uses the Bioconductor mixOmics package. Objects
  created with previous versions of `recipes` can still use
  [`juice()`](https://recipes.tidymodels.org/dev/reference/juice.md) and
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md). With
  the current version, the categorical outcomes can be used but now
  multivariate models do not. Also, the new method allows for sparse
  results.

- As suggested by [@StefanBRas](https://github.com/StefanBRas),
  [`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md)
  now defaults to the C engine
  ([\#518](https://github.com/tidymodels/recipes/issues/518))

- Avoided partial matching on [`seq()`](https://rdrr.io/r/base/seq.html)
  arguments in internal functions.

- Improved error messaging, for example when a user tries to
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) a
  tuneable recipe.

- `step_upsample()` and `step_downsample()` are soft deprecated in
  recipes as they are now available in the themis package. They will be
  removed in the next version.

- [`step_zv()`](https://recipes.tidymodels.org/dev/reference/step_zv.md)
  now handles `NA` values so that variables with zero variance *plus*
  are removed.

- The selectors
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) and
  [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html) can
  now be used in step selections
  ([\#477](https://github.com/tidymodels/recipes/issues/477)).

- The `tune` pacakge can now use recipes with `check` operations (but
  also requires `tune` \>= 0.1.0.9000).

- The `tidy` method for
  [`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md)
  now has an option for returning the variance statistics for each
  component.

## recipes 0.1.12

CRAN release: 2020-05-01

- Some S3 methods were not being registered previously. This caused
  issues in R 4.0.

## recipes 0.1.11

CRAN release: 2020-04-30

### Other Changes

- While `recipes` does not directly depend on `dials`, it has several S3
  methods for generics in `dials`. Version 0.0.5 of `dials` added
  stricter validation for these methods, so changes were required for
  `recipes`.

### New Operations

- [`step_cut()`](https://recipes.tidymodels.org/dev/reference/step_cut.md)
  enables you to create a factor from a numeric based on provided break
  (contributed by Edwin Thoen)

## recipes 0.1.10

CRAN release: 2020-03-18

### Breaking Changes

- renamed `yj_trans()` to
  [`yj_transform()`](https://recipes.tidymodels.org/dev/reference/recipes-internal.md)
  to avoid conflicts.

### Other Changes

- Added flexible naming options for new columns created by
  [`step_depth()`](https://recipes.tidymodels.org/dev/reference/step_depth.md)
  and
  [`step_classdist()`](https://recipes.tidymodels.org/dev/reference/step_classdist.md)
  ([\#262](https://github.com/tidymodels/recipes/issues/262)).

- Small changes for base R’s `stringsAsFactors` change.

## recipes 0.1.9

CRAN release: 2020-01-07

- Delayed S3 method registration for `tune::tunable()` methods that live
  in recipes will now work correctly on R \>=4.0.0
  ([\#439](https://github.com/tidymodels/recipes/issues/439),
  [tidymodels/tune#146](https://github.com/tidymodels/tune/issues/146)).

- [`step_relevel()`](https://recipes.tidymodels.org/dev/reference/step_relevel.md)
  added.

## `recipes` 0.1.8

CRAN release: 2019-12-18

### Breaking Changes

- The imputation steps do not change the data type being imputed now.
  Previously, if the data were integer, the data would be changed to
  numeric (for some step types). The change is breaking since the
  underlying data of imputed values are now saved as a list instead of a
  vector (for some step types).

- The data sets were moved to the new `modeldata` package.

- [`step_num2factor()`](https://recipes.tidymodels.org/dev/reference/step_num2factor.md)
  was rewritten due to a bug that ignored the user-supplied levels
  ([\#425](https://github.com/tidymodels/recipes/issues/425)). The
  results of the `transform` argument are now required to be a function
  and `levels` must now be supplied.

### Other Changes

- Using a minus in the formula to
  [`recipes()`](https://recipes.tidymodels.org/dev/reference/recipes.md)
  is no longer allowed (it didn’t remove variables anyway).
  [`step_rm()`](https://recipes.tidymodels.org/dev/reference/step_rm.md)
  or
  [`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
  can be used instead.

- When using a selector that returns no columns,
  [`juice()`](https://recipes.tidymodels.org/dev/reference/juice.md) and
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) will
  now return a tibble with as many rows as the original template data or
  the `new_data` respectively. This is more consistent with how
  selectors work in dplyr
  ([\#411](https://github.com/tidymodels/recipes/issues/411)).

- Code was added to explicitly register `tunable` methods when `recipes`
  is loaded. This is required because of changes occurring in R 4.0.

- [`check_class()`](https://recipes.tidymodels.org/dev/reference/check_class.md)
  checks if a variable is of the designated class. Class is either
  learned from the train set or provided in the check. (contributed by
  Edwin Thoen)

- [`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md)
  and
  [`step_scale()`](https://recipes.tidymodels.org/dev/reference/step_scale.md)
  gained a `factor` argument with values of 1 or 2 that can scale the
  standard deviations used to transform the data.
  ([\#380](https://github.com/tidymodels/recipes/issues/380))

- [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) now
  produces a tibble with columns in the same order as
  [`juice()`](https://recipes.tidymodels.org/dev/reference/juice.md)
  ([\#365](https://github.com/tidymodels/recipes/issues/365))

## `recipes` 0.1.7

CRAN release: 2019-09-15

Release driven by changes in `tidyr` (v 1.0.0).

### Breaking Changes

`format_selector()`’s `wdth` argument has been renamed to `width`
([\#250](https://github.com/tidymodels/recipes/issues/250)).

### New Operations

- [`step_mutate_at()`](https://recipes.tidymodels.org/dev/reference/step_mutate_at.md),
  [`step_rename()`](https://recipes.tidymodels.org/dev/reference/step_rename.md),
  and
  [`step_rename_at()`](https://recipes.tidymodels.org/dev/reference/step_rename_at.md)
  were added.

### Other Changes

- The use of
  [`varying()`](https://parsnip.tidymodels.org/reference/varying.html)
  will be deprecated in favor of an upcoming function
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html). No
  changes are need in this version, but subsequent versions will work
  with [`tune()`](https://hardhat.tidymodels.org/reference/tune.html).

- [`format_ch_vec()`](https://recipes.tidymodels.org/dev/reference/format_ch_vec.md)
  and `format_selector()` are now exported
  ([\#250](https://github.com/tidymodels/recipes/issues/250)).

- `check_new_values` breaks `bake` if variable contains values that were
  not observed in the train set (contributed by Edwin Thoen)

- When no outcomes are in the recipe, using
  `juice(object, all_outcomes()` and
  `bake(object, new_data, all_outcomes()` will return a tibble with zero
  rows and zero columns (instead of failing).
  ([\#298](https://github.com/tidymodels/recipes/issues/298)). This will
  also occur when the selectors select no columns.

- As alternatives to
  [`step_kpca()`](https://recipes.tidymodels.org/dev/reference/step_kpca.md),
  two separate steps were added called
  [`step_kpca_rbf()`](https://recipes.tidymodels.org/dev/reference/step_kpca_rbf.md)
  and
  [`step_kpca_poly()`](https://recipes.tidymodels.org/dev/reference/step_kpca_poly.md).
  The use of
  [`step_kpca()`](https://recipes.tidymodels.org/dev/reference/step_kpca.md)
  will print a deprecation message that it will be going away.

- [`step_nzv()`](https://recipes.tidymodels.org/dev/reference/step_nzv.md)
  and
  [`step_poly()`](https://recipes.tidymodels.org/dev/reference/step_poly.md)
  had arguments promoted out of their `options` slot. `options` can be
  used in the short term but is deprecated.

- `step_downsample()` will replace the `ratio` argument with
  `under_ratio` and `step_upsample()` will replace it with `over_ratio`.
  `ratio` still works (for now) but issues a deprecation message.

- [`step_discretize()`](https://recipes.tidymodels.org/dev/reference/step_discretize.md)
  has arguments moved out of `options` too; the main arguments are now
  `num_breaks` (instead of `cuts`) and `min_unique`. Again, deprecation
  messages are issued with the old argument structure.

- Models using the `dimRed` package
  ([`step_kpca()`](https://recipes.tidymodels.org/dev/reference/step_kpca.md),
  [`step_isomap()`](https://recipes.tidymodels.org/dev/reference/step_isomap.md),
  and
  [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md))
  would silently fail if the projection method failed. An error is
  issued now.

- Methods were added for a future generic called
  [`tunable()`](https://generics.r-lib.org/reference/tunable.html). This
  outlines which parameters in a step can/could be tuned.

## `recipes` 0.1.6

CRAN release: 2019-07-02

Release driven by changes in `rlang`.

### Breaking Changes

- Since 2018, a warning has been issued when the wrong argument was used
  in `bake(recipe, newdata)`. The depredation period is over and
  `new_data` is officially required.

- Previously, if
  [`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md)
  did *not* collapse any levels, it would still add an “other” level to
  the factor. This would lump new factor levels into “other” when data
  were baked (as
  [`step_novel()`](https://recipes.tidymodels.org/dev/reference/step_novel.md)
  does). This no longer occurs since it was inconsistent with
  [`?step_other`](https://recipes.tidymodels.org/dev/reference/step_other.md),
  which said that

> “If no pooling is done the data are unmodified”.

### New Operations

- [`step_normalize()`](https://recipes.tidymodels.org/dev/reference/step_normalize.md)
  centers and scales the data (if you are, like Max, too lazy to use two
  separate steps).
- [`step_unknown()`](https://recipes.tidymodels.org/dev/reference/step_unknown.md)
  will convert missing data in categorical columns to “unknown” and
  update factor levels.

### Other Changes

- If `threshold` argument of `step_other` is greater than one then it
  specifies the minimum sample size before the levels of the factor are
  collapsed into the “other” category.
  [\#289](https://github.com/tidymodels/recipes/issues/289)

- [`step_knnimpute()`](https://recipes.tidymodels.org/dev/reference/step_knnimpute.md)
  can now pass two options to the underlying knn code, including the
  number of threads
  ([\#323](https://github.com/tidymodels/recipes/issues/323)).

- Due to changes by CRAN,
  [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md)
  only works on versions of R \>= 3.6.0 due to dependency issues.

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  and
  [`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md)
  are now tolerant to cases where that step’s selectors do not capture
  any columns. In this case, no modifications to the data are made.
  ([\#290](https://github.com/tidymodels/recipes/issues/290),
  [\#348](https://github.com/tidymodels/recipes/issues/348))

- [`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
  can now retain the original columns that are used to make the dummy
  variables. ([\#328](https://github.com/tidymodels/recipes/issues/328))

- [`step_other()`](https://recipes.tidymodels.org/dev/reference/step_other.md)’s
  print method only reports the variables with collapsed levels (as
  opposed to any column that was *tested* to see if it needed
  collapsing).
  ([\#338](https://github.com/tidymodels/recipes/issues/338))

- [`step_pca()`](https://recipes.tidymodels.org/dev/reference/step_pca.md),
  [`step_kpca()`](https://recipes.tidymodels.org/dev/reference/step_kpca.md),
  [`step_ica()`](https://recipes.tidymodels.org/dev/reference/step_ica.md),
  [`step_nnmf()`](https://recipes.tidymodels.org/dev/reference/step_nnmf.md),
  [`step_pls()`](https://recipes.tidymodels.org/dev/reference/step_pls.md),
  and
  [`step_isomap()`](https://recipes.tidymodels.org/dev/reference/step_isomap.md)
  now accept zero components. In this case, the original data are
  returned.

## `recipes` 0.1.5

CRAN release: 2019-03-21

Small release driven by changes in
[`sample()`](https://rdrr.io/r/base/sample.html) in the current r-devel.

### Other Changes

- A new vignette discussing roles has been added.

- To provide infrastructure for finalizing varying parameters, an
  [`update()`](https://rdrr.io/r/stats/update.html) method for recipe
  steps has been added. This allows users to alter information in steps
  that have not yet been trained.

- `step_interact` will no longer fail if an interaction contains an
  interaction using column that has been previously filtered from the
  data. A warning is issued when this happens and no interaction terms
  will be created.

- `step_corr` was made more fault tolerant for cases where the data
  contain a zero-variance column or columns with missing values.

- Set the embedded environment to NULL in `prep.step_dummy` to reduce
  the file size of serialized recipe class objects when using `saveRDS`.

### Breaking Changes

- The `tidy` method for `step_dummy` now returns the original variable
  *and* the levels of the future dummy variables.

### Bug Fixes

- Updating the role of new columns generated by a recipe step no longer
  also updates `NA` roles of existing columns
  ([\#296](https://github.com/tidymodels/recipes/issues/296)).

## `recipes` 0.1.4

CRAN release: 2018-11-19

### Breaking Changes

- Several argument names were changed to be consistent with other
  `tidymodels` packages (e.g. `dials`) and the general tidyverse naming
  conventions.

  - `K` in `step_knnimpute` was changed to `neighbors`. `step_isomap`
    had the number of neighbors promoted to a main argument called
    `neighbors`
  - `step_pca`, `step_pls`, `step_kpca`, `step_ica` now use `num_comp`
    instead of `num`. , `step_isomap` uses `num_terms` instead of `num`.
  - `step_bagimpute` moved `nbagg` out of the options and into a main
    argument `trees`.
  - `step_bs` and `step_ns` has degrees of freedom promoted to a main
    argument with name `deg_free`. Also, `step_bs` had `degree` promoted
    to a main argument.
  - `step_BoxCox` and `step_YeoJohnson` had `nunique` change to
    `num_unique`.
  - `bake`, `juice` and other functions has `newdata` changed to
    `new_data`. For *this version only*, using `newdata` will only
    result in a wanring.
  - Several steps had `na.rm` changed to `na_rm`.
  - `prep` and a few steps had `stringsAsFactors` changed to
    `strings_as_factors`.

- [`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
  can now only add *new* additional roles. To alter existing roles, use
  [`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md).
  This change also allows for the possibility of having multiple
  roles/types for one variable.
  [\#221](https://github.com/tidymodels/recipes/issues/221)

- All steps gain an `id` field that will be used in the future to
  reference other steps.

- The `retain` option to `prep` is now defaulted to `TRUE`. If
  `verbose = TRUE`, the approximate size of the data set is printed.
  [\#207](https://github.com/tidymodels/recipes/issues/207)

### New Operations

- `step_integer` converts data to ordered integers similar to
  [`LabelEncoder`](https://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.LabelEncoder.html)
  [\#123](https://github.com/tidymodels/recipes/issues/123) and
  [\#185](https://github.com/tidymodels/recipes/issues/185)
- `step_geodist` can be used to calculate the distance between geocodes
  and a single reference location.
- `step_arrange`, `step_filter`, `step_mutate`, `step_sample`, and
  `step_slice` implement their `dplyr` analogs.
- `step_nnmf` computes the non-negative matrix factorization for data.

### Other Changes

- The `rsample` function `prepper` was moved to `recipes`
  [(issue)](https://github.com/tidymodels/rsample/issues/48).
- A number of packages were moved from “Imports” to “Suggests” to reduce
  the install footprint. A function was added to prompt the user to
  install the needed packages when the relevant steps are invoked.
- `step_step_string2factor` will [now accept
  factors](https://github.com/tidymodels/recipes/issues/174) and leave
  them as-is.
- `step_knnimpute` now excludes missing data in the variable to be
  imputed from the nearest-neighbor calculation. This would have
  resulted in some missing data to not be imputed (i.e. return another
  missing value).
- `step_dummy` now produces a warning (instead of failing) when
  non-factor columns are selected. Only factor columns are used; no
  conversion is done for character data.
  [issue](https://github.com/tidymodels/recipes/issues/186)
  [\#186](https://github.com/tidymodels/recipes/issues/186)
- `dummy_names` gained a separator argument.
  [issue](https://github.com/tidymodels/recipes/issues/183)
  [\#183](https://github.com/tidymodels/recipes/issues/183)
- `step_downsample` and `step_upsample` now have `seed` arguments for
  more control over randomness.
- `broom` is no longer used to get the `tidy` generic. These are now
  contained in the `generics` package.
- When a recipe is prepared, a running list of all columns is created
  and the last known use of each column is kept. This is to avoid bugs
  when a step that is skipped removes columns.
  [issue](https://github.com/tidymodels/recipes/issues/239)
  [\#239](https://github.com/tidymodels/recipes/issues/239)

## `recipes` 0.1.3

CRAN release: 2018-06-16

### New Operations

- `check_range` breaks `bake` if variable range in new data is outside
  the range that was learned from the train set (contributed by Edwin
  Thoen)

- `step_lag` can lag variables in the data set (contributed by Alex
  Hayes).

- `step_naomit` removes rows with missing data for specific columns
  (contributed by Alex Hayes).

- `step_rollimpute` can be used to impute data in a sequence or series
  by estimating their values within a moving window.

- `step_pls` can conduct supervised feature extraction for predictors.

### Other Changes

- `step_log` gained an `offset` argument.

- `step_log` gained a `signed` argument (contributed by Edwin Thoen).

- The internal functions `sel2char` and `printer` have been exported to
  enable [other packages to contain
  steps](https://github.com/tidymodels/recipes/issues/122).

- When training *new* steps after some steps have been previously
  trained, the `retain = TRUE` option should be set on [previous
  invocations of
  `prep`](https://github.com/tidymodels/recipes/issues/143).

- For `step_dummy`:

  - It can now compute the [entire set of dummy
    variables](https://github.com/tidymodels/recipes/issues/145) per
    factor predictor using the `one_hot = TRUE` option. Thanks to Davis
    Vaughan.
  - The `contrast` option was removed. The step uses the global option
    for contrasts.
  - \`The step also produces missing indicator variables when the
    original factor [has a missing
    value](https://github.com/tidymodels/recipes/issues/133)

- `step_other` will now convert novel levels of the factor to the
  “other” level.

- `step_bin2factor` now has an option to choose [how the values are
  translated to the
  levels](https://github.com/tidymodels/recipes/issues/142) (contributed
  by Michael Levy).

- `bake` and `juice` can now export basic data frames.

- The `okc` data were updated with two additional columns.

### Bug Fixes

- [issue 125](https://github.com/tidymodels/recipes/issues/125) that
  prevented several steps from working with **dplyr** grouped data
  frames. (contributed by Jeffrey Arnold)

- [issue 127](https://github.com/tidymodels/recipes/issues/127) where
  options to `step_discretize` were not being passed to `discretize`.

## `recipes` 0.1.2

CRAN release: 2018-01-11

### General Changes

- Edwin Thoen suggested [adding validation
  checks](https://github.com/tidymodels/recipes/pull/104) for certain
  data characteristics. This fed into the existing notion of expanding
  `recipes` beyond steps (see the [non-step steps
  project](https://github.com/tidymodels/recipes/projects)). A new set
  of operations, called **`checks`**, can now be used. These should
  throw an informative error when the check conditions are not met and
  return the existing data otherwise.

- Steps now have a `skip` option that will not apply preprocessing when
  `bake` is used. See the article [on skipping
  steps](https://recipes.tidymodels.org/articles/Skipping.html) for more
  information.

### New Operations

- `check_missing` will validate that none of the specified variables
  contain missing data.

- `detect_step` can be used to check if a recipe contains a particular
  preprocessing operation.

- `step_num2factor` can be used to convert numeric data (especially
  integers) to factors.

- `step_novel` adds a new factor level to nominal variables that will be
  used when new data contain a level that did not exist when the recipe
  was prepared.

- `step_profile` can be used to generate design matrix grids for
  prediction profile plots of additive models where one variable is
  varied over a grid and all of the others are fixed at a single value.

- `step_downsample` and `step_upsample` can be used to change the number
  of rows in the data based on the frequency distributions of a factor
  variable in the training set. By default, this operation is only
  applied to the training set; `bake` ignores this operation.

- `step_naomit` drops rows when specified columns contain `NA`, similar
  to
  [`tidyr::drop_na`](https://tidyr.tidyverse.org/reference/drop_na.html).

- `step_lag` allows for the creation of lagged predictor columns.

### Other Changes

- `step_spatialsign` now has the option of removing missing data prior
  to computing the norm.

## `recipes` 0.1.1

CRAN release: 2017-11-20

- The default selectors for `bake` was changed from
  [`all_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
  to
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).
- The `verbose` option for `prep` is now defaulted to `FALSE`
- [A bug in
  `step_dummy`](https://github.com/tidymodels/recipes/issues/83) was
  fixed that makes sure that the correct binary variables are generated
  despite the levels or values of the incoming factor. Also,
  `step_dummy` now requires factor inputs.
- `step_dummy` also has a new default naming function that works better
  for factors. However, there is an extra argument (`ordinal`) now to
  the functions that can be passed to `step_dummy`.  
- `step_interact` now allows for selectors
  (e.g. [`all_predictors()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
  or `starts_with("prefix")` to be used in the interaction formula.
- `step_YeoJohnson` gained an `na.rm` option.
- [`dplyr::one_of`](https://github.com/tidymodels/recipes/issues/85) was
  added to the list of selectors.
- `step_bs` adds B-spline basis functions.
- `step_unorder` converts ordered factors to unordered factors.
- `step_count` counts the number of instances that a pattern exists in a
  string.
- `step_string2factor` and `step_factor2string` can be used to move
  between encodings.
- `step_lowerimpute` is for numeric data where the values cannot be
  measured below a specific value. For these cases, random uniform
  values are used for the truncated values.  
- A step to remove simple zero-variance variables was added (`step_zv`).
- A series of `tidy` methods were added for recipes and many (but not
  all) steps.
- In `bake.recipe`, the argument `newdata` is now without a default.
- `bake` and `juice` can now save the *final* processed data set in
  [sparse format](https://github.com/tidymodels/recipes/issues/49). Note
  that, as the steps are processed, a non-sparse data frame is used to
  store the results.
- A formula method was added for recipes to get a formula with the
  outcome(s) and predictors based on the trained recipe.

## `recipes` 0.1.0

CRAN release: 2017-07-27

First CRAN release.

- Changed `prepare` to `prep` per
  [issue](https://github.com/tidymodels/recipes/issues/59)
  [\#59](https://github.com/tidymodels/recipes/issues/59)

## `recipes` 0.0.1.9003

- Two of the main functions [changed
  names](https://github.com/tidymodels/recipes/issues/57). `learn` has
  become `prepare` and `process` has become `bake`

## `recipes` 0.0.1.9002

### New steps

- `step_lincomb` removes variables involved in linear combinations to
  resolve them.
- A step for converting binary variables to factors (`step_bin2factor`)
- `step_regex` applies a regular expression to a character or factor
  vector to create dummy variables.

### Other changes

- `step_dummy` and `step_interact` do a better job of respecting missing
  values in the data set.

## `recipes` 0.0.1.9001

- The class system for `recipe` objects was changed so that [pipes can
  be used to create the recipe with a
  formula](https://github.com/tidymodels/recipes/issues/46).
- `process.recipe` lost the `role` argument in factor of a general set
  of
  [selectors](https://recipes.tidymodels.org/articles/Selecting_Variables.html).
  If no selector is used, all the predictors are returned.
- Two steps for simple imputation using the mean or mode were added.
