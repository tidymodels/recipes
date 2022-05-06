# bad data

    Code
      rec %>% step_impute_lower(carbon, hydrogen, has_neg) %>% prep()
    Condition
      Error in `prep()`:
      ! Some columns have negative values. Lower bound imputation is intended for data bounded at zero.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          3

---

    Code
      prep(rec2)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          3
      
      Training data contained 536 data points and no missing data.
      
      Operations:
      
      Lower bound imputation for carbon, hydrogen [trained]

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
      
      Lower bound imputation for <none>

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
      
      Lower bound imputation for <none> [trained]

