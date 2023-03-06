# nocov start

# Global vars ------------------------------------------------------------------

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

# nocov end
