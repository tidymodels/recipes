# quasiquotation

    Code
      prep(rec_1, training = iris %>% slice(1:75))
    Condition
      Error in `step_filter()`:
      Caused by error in `dplyr::filter()`:
      i In argument: `Species %in% values`.
      Caused by error in `Species %in% values`:
      ! object 'values' not found

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Row filtering using: Sepal.Length > 4.5

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Row filtering using: ~Sepal.Length > 4.5 | Trained

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
      * Row filtering using: <none>

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
      * Row filtering using: <none> | Trained

