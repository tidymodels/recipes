#' @importFrom stats sd predict complete.cases median quantile as.formula model.frame
#' @importFrom stats binomial prcomp model.matrix terms poly cov var optimize
#' @importFrom stats mahalanobis runif cor na.pass
#' @importFrom purrr map map_dbl map_lgl map_chr map_df map2_df map_dfc reduce
#' @importFrom purrr map_dfr map_if
#' @importFrom ipred ipredbagg
#' @importFrom tibble as_tibble add_column is_tibble tibble
#' @importFrom dplyr filter group_by count ungroup do select_vars tbl_vars mutate
#' @importFrom dplyr tibble bind_rows slice right_join rename select full_join
#' @importFrom dplyr arrange desc bind_cols sample_n sample_frac mutate_at
#' @importFrom Matrix Matrix
#' @importFrom rlang quos call2 sym quo_get_expr quo_text expr f_lhs f_rhs
#' @importFrom rlang is_empty is_quosure as_character na_dbl syms !! names2
#' @importFrom rlang quo quo_squash exec na_dbl
#' @importFrom gower gower_topn
#' @importFrom lubridate year yday week decimal_date quarter semester wday month
#' @importFrom lubridate is.Date
#' @importFrom utils stack globalVariables packageVersion object.size install.packages
#' @importFrom tidyselect everything
#' @importFrom withr with_seed
#' @importFrom splines ns bs

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    ".", ".orig_order",                                                # roles.R
    "type", "new_type",                                                 # misc.R
    "variable",                                                        # novel.R
    "estimate",                                                  # lowerimpute.R
    ".row"                                                           # integer.R
  )
)

# ------------------------------------------------------------------------------

# nocov start
.onLoad <- function(libname, pkgname) {
  # This package has specific methods for the `tunable` generic. That generic
  # is defined in the `tune` package. As of R 4.0, we need to register them.
  recipe_exports <- getNamespaceExports(ns = "recipes")
  tunable_steps <- grep("tunable.step", recipe_exports, fixed = TRUE, value = TRUE)
  for (i in tunable_steps) {
    s3_register("dplyr::tune", i)
  }
}

s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}

# nocov end

