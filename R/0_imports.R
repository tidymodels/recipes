#' @importFrom stats sd predict complete.cases median quantile as.formula model.frame
#' @importFrom stats binomial prcomp model.matrix terms poly cov var optimize
#' @importFrom stats mahalanobis runif cor na.pass na.omit lm
#' @importFrom purrr map map_dbl map_lgl map_chr map_df map2_df map_dfc reduce
#' @importFrom purrr map_dfr map_if
#' @importFrom ipred ipredbagg
#' @importFrom tibble as_tibble add_column is_tibble tibble
#' @importFrom dplyr filter group_by count ungroup do select_vars tbl_vars mutate
#' @importFrom dplyr tibble bind_rows slice right_join rename select full_join
#' @importFrom dplyr arrange desc bind_cols sample_n sample_frac mutate_at
#' @importFrom dplyr summarise
#' @importFrom Matrix Matrix
#' @importFrom rlang quos call2 sym quo_get_expr quo_text expr f_lhs f_rhs
#' @importFrom rlang is_empty is_quosure as_character na_dbl syms !! names2
#' @importFrom rlang quo quo_squash exec na_dbl
#' @importFrom gower gower_topn
#' @importFrom lubridate year yday week decimal_date quarter semester wday month
#' @importFrom lubridate is.Date
#' @importFrom lifecycle deprecated
#' @importFrom utils stack globalVariables packageVersion object.size install.packages
#' @importFrom tidyselect everything
#' @importFrom withr with_seed
#' @importFrom splines ns bs
#' @importFrom vctrs vec_cast vec_slice

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
    ".order_1", ".order_2", "role", "skip"                           # recipes.R
  )
)
