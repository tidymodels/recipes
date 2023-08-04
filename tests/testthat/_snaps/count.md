# bad selector(s)

    Code
      rec %>% step_count(description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_count()`:
      ! For this step, only a single selector can be used.

---

    Code
      prep(rec2, training = covers)
    Condition
      Error in `step_count()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_count()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  Sepal.Width

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
      * Regular expression counts using: <none>

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
      * Regular expression counts using: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      'keep_original_cols' was added to `step_count()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Regular expression counts using: description

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 40 data points and no incomplete rows.
      
      -- Operations 
      * Regular expression counts using: description | Trained

