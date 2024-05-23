# bad args

    Code
      recipe(~., data = example_data) %>% step_impute_roll(all_predictors(), window = 3) %>%
        prep(training = example_data)
    Condition
      Error in `step_impute_roll()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double.
      * 1 date variable found: `day`

---

    Code
      recipe(~., data = example_data) %>% update_role(day, new_role = "time_index") %>%
        step_impute_roll(all_predictors(), window = 4) %>% prep(training = example_data)
    Condition
      Error in `step_impute_roll()`:
      ! `window` should be an odd integer >= 3.

---

    Code
      recipe(~., data = example_data) %>% update_role(day, new_role = "time_index") %>%
        step_impute_roll(all_predictors(), window = 3) %>% prep(training = example_data)
    Condition
      Error in `step_impute_roll()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double.
      * 1 integer variable found: `x4`

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

# printing

    Code
      print(rec)
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
      prep(rec)
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

