# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_holiday()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `day_Easter`

# error on incorrect holidays argument

    Code
      step_holiday(recipe(~., mtcars), holidays = c("Invalid Holiday", "NewYearsDay"))
    Condition
      Error in `step_holiday()`:
      ! Invalid `holidays` value. See `timeDate::listHolidays()` for possible values.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(holiday_rec, exp_dates[, 2, drop = FALSE])
    Condition
      Error in `step_holiday()`:
      ! The following required column is missing from `new_data`: day.

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
      * Holiday features from: <none>

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
      * Holiday features from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_holiday()` after this recipe was created.
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
      * Holiday features from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 366 data points and 1 incomplete row.
      
      -- Operations 
      * Holiday features from: day | Trained

