#' @importFrom dplyr arrange bind_cols bind_rows count desc do filter full_join
#' @importFrom dplyr group_by mutate mutate_at rename sample_frac sample_n
#' @importFrom dplyr select slice summarise ungroup
#' @importFrom gower gower_topn
#' @importFrom ipred ipredbagg
#' @importFrom lifecycle deprecated
#' @importFrom lubridate decimal_date is.Date month quarter semester wday week
#' @importFrom lubridate yday year hour minute second am
#' @importFrom Matrix Matrix
#' @importFrom purrr map map_chr map_dbl map_dfr map_if map_lgl reduce
#' @importFrom rlang !! as_character call2 exec expr f_lhs f_rhs is_empty
#' @importFrom rlang is_quosure na_dbl quo quo_get_expr quo_squash quo_text quos
#' @importFrom rlang sym syms
#' @importFrom splines bs ns
#' @importFrom stats as.formula binomial complete.cases cor cov lm mahalanobis
#' @importFrom stats median model.frame model.matrix na.omit na.pass optimize
#' @importFrom stats poly prcomp predict quantile runif sd terms var cov.wt
#' @importFrom tibble add_column as_tibble is_tibble tibble
#' @importFrom tidyselect everything
#' @importFrom utils globalVariables install.packages object.size packageVersion
#' @importFrom utils stack
#' @importFrom vctrs vec_cast vec_slice
#' @importFrom withr with_seed

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    ".", ".orig_order",                                                # roles.R
    "type", "new_type",                                                 # misc.R
    "variable",                                                        # novel.R
    "estimate",                                                  # lowerimpute.R
    ".row",                                                          # integer.R
    "denom",                                                           # ratio.R
    "component", "denom", "id", "value",                                 # ica.R
    "training", "x_names", "y_names",                                    # pls.R
    ".order_1", ".order_2", "role", "skip",                          # recipes.R
    "call_info",                                                     # extract.R
    "dat"                                                        # nnmf_sparse.R
  )
)
