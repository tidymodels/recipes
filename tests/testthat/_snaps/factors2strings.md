# bad args

    Code
      rec %>% step_factor2string(w, x) %>% prep(ex_dat, strings_as_factors = FALSE)
    Condition
      Error in `step_factor2string()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be factor, or ordered.

# printing

    Code
      print(ex_3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Training data contained 200 data points and no missing data.
      
      Operations:
      
      Character variables from y, z [trained]

---

    Code
      prep(ex_3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Training data contained 200 data points and no missing data.
      
      Operations:
      
      Character variables from y, z [trained]

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

