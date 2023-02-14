# bad args

    Code
      recipe(~., data = example_data) %>% step_impute_roll(all_predictors(), window = 3) %>%
        prep(training = example_data)
    Condition
      Error in `step_impute_roll()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double.

---

    Code
      recipe(~., data = example_data) %>% update_role(day, new_role = "time_index") %>%
        step_impute_roll(all_predictors(), window = 4) %>% prep(training = example_data)
    Condition
      Error in `step_impute_roll()`:
      ! `window` should be an odd integer >= 3

---

    Code
      recipe(~., data = example_data) %>% update_role(day, new_role = "time_index") %>%
        step_impute_roll(all_predictors(), window = 3) %>% prep(training = example_data)
    Condition
      Error in `step_impute_roll()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double.

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_rollimpute()
    Condition
      Error:
      ! `step_rollimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_roll()` instead.

# printing

    Code
      print(seven_pt)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:  3
      time_index: 1
      
      -- Operations 
      * Rolling imputation for: all_predictors()

---

    Code
      prep(seven_pt)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:  3
      time_index: 1
      
      -- Training information 
      Training data contained 12 data points and 7 incomplete rows.
      
      -- Operations 
      * Rolling imputation for: x1, x2, x3 | Trained

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Rolling imputation for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Rolling imputation for: <none> | Trained

