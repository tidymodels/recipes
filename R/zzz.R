# nocov start
# takes after https://raw.githubusercontent.com/r-lib/vctrs/master/R/zzz.R
.onLoad <- function(libname, pkgname) {

  s3_register("stats::update", "step")

}

# nocov end
