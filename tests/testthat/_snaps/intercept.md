# deals with bad input

    Code
      prep(step_intercept(recipe(~., data = ex_dat), value = "Pie"))
    Condition
      Error in `step_intercept()`:
      ! `value` must be a number, not the string "Pie".

---

    Code
      prep(step_intercept(recipe(~., data = ex_dat), name = 4))
    Condition
      Error in `step_intercept()`:
      ! `name` must be a single string, not the number 4.

---

    Code
      prep(step_intercept(recipe(~., data = ex_dat), all_predictors()))
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

