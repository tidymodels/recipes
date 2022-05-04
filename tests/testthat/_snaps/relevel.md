# bad args

    Code
      rec %>% step_relevel(sqft, ref_level = 23) %>% prep()
    Condition
      Error in `prep()`:
      ! Columns must be character or factor: 

---

    Code
      rec %>% step_relevel(city, ref_level = "missing_level") %>% prep()
    Condition
      Error in `error_cnd()`:
      ! Conditions must have named data fields

# printing

    Code
      print(rec %>% step_relevel(zip, ref_level = "z95838"))
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          9
      
      Operations:
      
      Re-order factor level to ref_level for zip

---

    Code
      print(rec %>% step_relevel(zip, ref_level = "z95838") %>% prep())
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          9
      
      Training data contained 800 data points and no missing data.
      
      Operations:
      
      Re-order factor level to ref_level for zip [trained]

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
      
      Re-order factor level to ref_level for <none>

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
      
      Re-order factor level to ref_level for <none> [trained]

