# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_time()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `time_hour`

# errors on wrong values of features

    Code
      prep(step_time(recipe(~times, examples), all_predictors(), features = "hourly"))
    Condition
      Error in `step_time()`:
      Caused by error in `prep()`:
      ! `features` must be one of "am", "hour", "hour12", "minute", "second", or "decimal_day", not "hourly".
      i Did you mean "hour12"?

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = examples[, -1])
    Condition
      Error in `step_time()`:
      ! The following required column is missing from `new_data`: times.

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
      * Time features from: <none>

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
      * Time features from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_time()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Time features from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 5 data points and no incomplete rows.
      
      -- Operations 
      * Time features from: times | Trained

