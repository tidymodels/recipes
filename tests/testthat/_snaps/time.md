# error with missing cols

    Code
      bake(date_rec, new_data = examples %>% select(-times))
    Condition
      Error in `check_bake_cols()`:
      ! Columns (`times`) are not present in new_data for step_date()

# printing

    Code
      print(date_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Time features from all_predictors()

---

    Code
      prep(date_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 5 data points and no missing data.
      
      Operations:
      
      Time features from times [trained]

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
      
      Time features from <none>

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
      
      Time features from <none> [trained]

