# tune_args() errors on multiple tune()s in same arg

    Code
      recipe(~., data = mtcars) %>% step_pca(all_predictors(), num_comp = ~ tune() +
        tune()) %>% tune_args()
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `purrr::map_chr()`:
      i In index: 1.
      i With name: num_comp.
      Caused by error in `find_tune_id()`:
      ! Only one tunable value is currently allowed per argument. The current argument has: tune() + tune().

# tune_tbl() errors on duplicate ids

    Code
      tune_tbl(name = c("a", "b"), tunable = c(TRUE, TRUE), id = c("a", "a"), source = c(
        "a", "b"), component = c("a", "b"), component_id = c("a", "b"), full = TRUE)
    Condition
      Error in `tune_tbl()`:
      ! There are duplicate id values listed in `tune()`: "a".

