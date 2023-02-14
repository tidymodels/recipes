# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("generics::tune_args", "recipe", tune_args_recipe)
  vctrs::s3_register("generics::tune_args", "step", tune_args_step)
  vctrs::s3_register("generics::tune_args", "check", tune_args_check)
}

# nocov end
