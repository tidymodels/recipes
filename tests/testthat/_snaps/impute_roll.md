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
      Caused by error in `prep()`:
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

---

    Code
      recipe(~., data = mtcars) %>% step_impute_roll(all_predictors(), statistic = mean,
      window = 1) %>% prep()
    Condition
      Error in `step_impute_roll()`:
      Caused by error in `prep()`:
      ! `window` must be a whole number larger than or equal to 3, not the number 1.

---

    Code
      recipe(~., data = mtcars) %>% step_impute_roll(all_predictors(), statistic = mean,
      window = 4) %>% prep()
    Condition
      Error in `step_impute_roll()`:
      Caused by error in `prep()`:
      ! `window` should be an odd integer >= 3.

---

    Code
      recipe(~., data = mtcars) %>% step_impute_roll(all_predictors(), statistic = NULL) %>%
        prep()
    Condition
      Error in `step_impute_roll()`:
      Caused by error in `prep()`:
      ! `statistic` must be a function, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(seven_pt, new_data = example_data[, c(-2)])
    Condition
      Error in `step_impute_roll()`:
      ! The following required column is missing from `new_data`: x1.

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

