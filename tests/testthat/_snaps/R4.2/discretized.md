# multiple column prefix

    Code
      recipe(~., data = example_data) %>% step_discretize(x1, x2, options = list(
        labels = "hello")) %>% prep()
    Condition
      Warning:
      Note that the options `prefix` and `labels` will be applied to all variables
      Error in `step_discretize()`:
      Caused by error in `cut.default()`:
      ! lengths of 'breaks' and 'labels' differ

