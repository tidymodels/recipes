# bad data

    Code
      rec %>% step_impute_lower(carbon, hydrogen, has_neg) %>% prep()
    Condition
      Error in `prep()`:
      ! Some columns have negative values. Lower bound imputation is intended for data bounded at zero.

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_lowerimpute()
    Condition
      Error:
      ! `step_lowerimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      Please use `step_impute_lower()` instead.

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

