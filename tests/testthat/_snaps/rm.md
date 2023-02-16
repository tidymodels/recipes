# basic rename

    Code
      prep(rec, training = iris %>% slice(1:75))
    Condition
      Error in `step_rm()`:
      Caused by error in `prep()`:
      ! Can't rename variables in this context.

# remove with quasi-quotation

    Code
      prep(rec_1, training = iris %>% slice(1:75))
    Condition
      Error in `step_rm()`:
      Caused by error in `prep()`:
      ! Problem while evaluating `all_of(sepal_vars)`.
      Caused by error in `as_indices_impl()`:
      ! object 'sepal_vars' not found

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Variables removed: x1

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Variables removed: x1 | Trained

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
      * Variables removed: <none>

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
      * Variables removed: <none> | Trained

