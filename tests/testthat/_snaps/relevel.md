# bad args

    Code
      rec %>% step_relevel(age, ref_level = 23) %>% prep()
    Condition
      Error in `prep()`:
      ! Columns must be character or factor: 

---

    Code
      rec %>% step_relevel(diet, ref_level = "missing_level") %>% prep()
    Condition
      Error in `error_cnd()`:
      ! Conditions must have named data fields

# printing

    Code
      print(rec %>% step_relevel(location, ref_level = "oakland"))
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          6
      
      Operations:
      
      Re-order factor level to ref_level for location

---

    Code
      print(rec %>% step_relevel(location, ref_level = "oakland") %>% prep())
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          6
      
      Training data contained 30000 data points and 12077 incomplete rows. 
      
      Operations:
      
      Re-order factor level to ref_level for location [trained]

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

