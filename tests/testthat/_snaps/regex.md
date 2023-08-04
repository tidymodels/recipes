# bad selector(s)

    Code
      rec %>% step_regex(description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_regex()`:
      ! For this step, at most a single selector can be used.

---

    Code
      prep(rec4, training = covers)
    Condition
      Error in `step_regex()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_regex()`:
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
      * Regular expression dummy variable using: "."

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
      * Regular expression dummy variable using: "." | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      'keep_original_cols' was added to `step_regex()` after this recipe was created.
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
      * Regular expression dummy variable using: "(rock|stony)"

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
      * Regular expression dummy variable using: "(rock|stony)" | Trained

