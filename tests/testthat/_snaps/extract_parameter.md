# rethrows error correctly from implementation

    Code
      params <- extract_parameter_set_dials(rec)
    Condition
      Error in `eval_call_info()`:
      ! Error when calling `num_comp()`: Error in dials::num_comp(range = c(1L, 4L)) : mocked error

