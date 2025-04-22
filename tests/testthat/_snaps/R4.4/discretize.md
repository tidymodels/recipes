# multiple column prefix

    Code
      prep(step_discretize(recipe(~., data = example_data), x1, x2, options = list(
        labels = "hello")))
    Condition
      Warning:
      Note that the options `prefix` and `labels` will be applied to all variables.
      Error in `step_discretize()`:
      Caused by error in `cut.default()`:
      ! number of intervals and length of 'labels' differ

