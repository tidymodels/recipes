# deals with bad input

    Code
      recipe(~., data = ex_dat) %>% step_intercept(value = "Pie") %>% prep()
    Condition
      Error in `step_intercept()`:
      ! `value` must be a number, not the string "Pie".

---

    Code
      recipe(~., data = ex_dat) %>% step_intercept(name = 4) %>% prep()
    Condition
      Error in `step_intercept()`:
      ! `name` must be a single string, not the number 4.

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
      ! Name collision occurred. The following variable names already exist:
      * `intercept`

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

