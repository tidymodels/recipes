# tune_args() errors on multiple tune()s in same arg

    Code
      tune_args(step_pca(recipe(~., data = mtcars), all_predictors(), num_comp = ~
      tune() + tune()))
    Condition
      Error in `find_tune_id()`:
      ! Only one tunable value is currently allowed per argument. The current argument has: tune() + tune().

# tune_tbl() errors on duplicate ids

    Code
      tune_tbl(name = c("a", "b"), tunable = c(TRUE, TRUE), id = c("a", "a"), source = c(
        "a", "b"), component = c("a", "b"), component_id = c("a", "b"), full = TRUE)
    Condition
      Error:
      ! There are duplicate id values listed in `tune()`: "a".

