# extract single parameter from recipe with no steps

    Code
      extract_parameter_dials(bare_rec, parameter = "none there")
    Condition
      Error in `extract_parameter_dials()`:
      ! No parameter exists with id "none there".

# extract single parameter from recipe with no tunable parameters

    Code
      extract_parameter_dials(rm_rec, parameter = "none there")
    Condition
      Error in `extract_parameter_dials()`:
      ! No parameter exists with id "none there".

