# check old PLS scores from recipes version <= 0.1.12

    Code
      new_values_te <- bake(old_pls, biom_te)
    Condition
      Warning:
      `keep_original_cols` was added to `step_pls()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

