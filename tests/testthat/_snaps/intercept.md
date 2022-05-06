# deals with bad input

    Code
      recipe(~., data = ex_dat) %>% step_intercept(value = "Pie") %>% prep()
    Condition
      Error in `step_intercept()`:
      ! Intercept value must be numeric.

---

    Code
      recipe(~., data = ex_dat) %>% step_intercept(name = 4) %>% prep()
    Condition
      Error in `step_intercept()`:
      ! Intercept/constant column name must be a character value.

---

    Code
      recipe(~., data = ex_dat) %>% step_intercept(all_predictors()) %>% prep()
    Condition
      Warning:
      Selectors are not used for this step.
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 10 data points and no missing data.
      
      Operations:
      
      Adding intercept named: intercept [trained]

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Adding intercept named: intercept

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 10 data points and no missing data.
      
      Operations:
      
      Adding intercept named: intercept [trained]

