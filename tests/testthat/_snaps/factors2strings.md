# bad args

    Code
      rec %>% step_factor2string(w, x) %>% prep(ex_dat, strings_as_factors = FALSE)
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be factor.

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Character variables from <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Character variables from <none> [trained]

