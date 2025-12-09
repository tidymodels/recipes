# Checks that steps have all S3 methods

This is a developer tool intended to help making sure all methods for
each step have been created.

## Usage

``` r
recipes_extension_check(
  pkg,
  exclude_steps = character(),
  exclude_methods = character()
)
```

## Arguments

- pkg:

  Character, name of package containing steps to check

- exclude_steps:

  Character, name of steps to exclude. This is mostly used to remove
  false positives.

- exclude_methods:

  Character, which methods to exclude testing for. Can take the values
  "prep", "bake", "print", "tidy", and "required_pkgs".

## Value

cli output

## Details

It is recommended that the following test in placed in packages that add
recipes steps to help keep everything up to date.

    test_that("recipes_extension_check", {
      expect_snapshot(
        recipes::recipes_extension_check(
          pkg = "pkgname"
        )
      )
    })

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

## Examples

``` r
recipes_extension_check(
  pkg = "recipes"
)
#> ℹ The following steps doesn't have `print.*` methods:
#> • step_testthat_helper
#> ℹ The following steps doesn't have `tidy.*` methods:
#> • step_testthat_helper
#> ℹ The following steps doesn't have `required_pkgs.*` methods:
#> • step_BoxCox
#> • step_YeoJohnson
#> • step_arrange
#> • step_bagimpute
#> • step_bin2factor
#> • step_bs
#> • step_center
#> • step_classdist
#> • step_corr
#> • step_count
#> • step_cut
#> • step_date
#> • step_discretize
#> • step_dummy
#> • step_dummy_extract
#> • step_dummy_multi_choice
#> • step_factor2string
#> • step_filter
#> • step_filter_missing
#> • step_geodist
#> • step_harmonic
#> • step_holiday
#> • step_hyperbolic
#> • step_impute_bag
#> • step_impute_knn
#> • step_impute_linear
#> • step_impute_lower
#> • step_impute_mean
#> • step_impute_median
#> • step_impute_mode
#> • step_impute_roll
#> • step_indicate_na
#> • step_integer
#> • step_interact
#> • step_intercept
#> • step_inverse
#> • step_invlogit
#> • step_knnimpute
#> • step_lag
#> • step_lincomb
#> • step_log
#> • step_logit
#> • step_lowerimpute
#> • step_meanimpute
#> • step_medianimpute
#> • step_modeimpute
#> • step_mutate_at
#> • step_naomit
#> • step_normalize
#> • step_novel
#> • step_ns
#> • step_num2factor
#> • step_nzv
#> • step_ordinalscore
#> • step_other
#> • step_pca
#> • step_percentile
#> • step_poly
#> • step_profile
#> • step_range
#> • step_ratio
#> • step_regex
#> • step_relevel
#> • step_relu
#> • step_rename
#> • step_rename_at
#> • step_rm
#> • step_rollimpute
#> • step_sample
#> • step_scale
#> • step_select
#> • step_shuffle
#> • step_slice
#> • step_spatialsign
#> • step_sqrt
#> • step_string2factor
#> • step_testthat_helper
#> • step_time
#> • step_unknown
#> • step_unorder
#> • step_zv

recipes_extension_check(
  pkg = "recipes",
  exclude_steps = "step_testthat_helper",
  exclude_methods = c("required_pkgs")
)
#> ✔ All steps have all method!
```
