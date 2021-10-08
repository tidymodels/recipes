# nocov start

.onLoad <- function(libname, pkgname) {

  # Can't use `rlang::is_installed()` at all, as that doesn't work when
  # called from `.onLoad()` for some reason. Instead, rely on `packageVersion()`
  # erroring when the package isn't installed.
  has_at_least_version <- function(pkg, version) {
    tryCatch(
      expr = utils::packageVersion(pkg) >= version,
      error = function(cnd) FALSE
    )
  }

  if (has_at_least_version("tune", "0.1.6.9001")) {
    # `tune_args.recipe()` and friends moved from tune to recipes
    vctrs::s3_register("generics::tune_args", "recipe", tune_args_recipe)
    vctrs::s3_register("generics::tune_args", "step", tune_args_step)
    vctrs::s3_register("generics::tune_args", "check", tune_args_check)
  }
}

# nocov end
