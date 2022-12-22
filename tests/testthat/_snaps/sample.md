# bad input

    Code
      iris_rec %>% step_sample(size = -1)
    Condition
      Error in `step_sample()`:
      ! `size` should be a positive number or NULL.

---

    Code
      iris_rec %>% step_sample(size = "a")
    Condition
      Error in `step_sample()`:
      ! `size` should be a positive number or NULL.

---

    Code
      iris_rec %>% step_sample(replace = "a")
    Condition
      Error in `step_sample()`:
      ! `replace` should be a single logical.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 6
      
      -- Operations 
      * Row sampling: <none>

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 6
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Row sampling: <none> | Trained

# sample with case weights

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    10
      case_weights:  1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Row sampling: <none> | Trained, weighted

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    10
      case_weights:  1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Row sampling: <none> | Trained, weighted

