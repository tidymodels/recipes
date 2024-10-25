# rethrows error correctly from implementation

    Code
      params <- extract_parameter_set_dials(rec)
    Condition
      Error in `mutate()`:
      i In argument: `object = purrr::map(call_info, eval_call_info)`.
      Caused by error in `purrr::map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Error when calling `num_comp()`: Error in dials::num_comp(range = c(1L, 4L)) : mocked error

