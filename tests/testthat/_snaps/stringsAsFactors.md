# strings_as_factors = FALSE

    Code
      rec1_as_str <- bake(rec1, new_data = as_str)
    Condition
      Warning:
       There were 2 columns that were factors when the recipe was prepped:
       'fact', 'ord'.
       This may cause errors when processing new data.

# strings_as_factors = TRUE

    Code
      rec2_as_str <- bake(rec2, new_data = as_str)
    Condition
      Warning:
       There were 2 columns that were factors when the recipe was prepped:
       'fact', 'ord'.
       This may cause errors when processing new data.

