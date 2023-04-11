# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_poly()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  mpg_poly_1

# printing

    Code
      print(with_poly)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * Orthogonal polynomials on: carbon, hydrogen

---

    Code
      prep(with_poly)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Orthogonal polynomials on: carbon, hydrogen | Trained

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
      * Orthogonal polynomials on: <none>

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
      * Orthogonal polynomials on: <none> | Trained

