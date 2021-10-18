# nocov start

.onLoad <- function(libname, pkgname) {

  # - If tune isn't installed, register the method (`packageVersion()` will error here)
  # - If tune >= 0.1.6.9001 is installed, register the method
  should_register_tune_args_method <- tryCatch(
    expr = utils::packageVersion("tune") >= "0.1.6.9001",
    error = function(cnd) TRUE
  )

  if (should_register_tune_args_method) {
    # `tune_args.recipe()` and friends moved from tune to recipes
    vctrs::s3_register("generics::tune_args", "recipe", tune_args_recipe)
    vctrs::s3_register("generics::tune_args", "step", tune_args_step)
    vctrs::s3_register("generics::tune_args", "check", tune_args_check)
  }
}

# nocov end
