# bad args

    Code
      rec %>% step_relevel(sqft, ref_level = 23) %>% prep()
    Condition
      Error in `step_relevel()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

---

    Code
      rec %>% step_relevel(city, ref_level = "missing_level") %>% prep()
    Condition
      Error in `step_relevel()`:
      Caused by error in `prep()`:
      ! Columns must contain the reference level 'missing_level': city

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

