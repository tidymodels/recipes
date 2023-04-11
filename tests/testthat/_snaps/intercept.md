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
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 10 data points and no incomplete rows.
      
      -- Operations 
      * Adding intercept named:: intercept | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_intercept()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  intercept

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Adding intercept named:: intercept

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 10 data points and no incomplete rows.
      
      -- Operations 
      * Adding intercept named:: intercept | Trained

