# simple Box Cox

    Code
      rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    Condition
      Warning:
      Non-positive values in selected variable.
      Warning:
      Fewer than `num_unique` values in selected variable.
      Warning:
      No Box-Cox transformation could be estimated for: `x2`, `x3`

# warnings

    Code
      recipe(~., data = exp_dat) %>% step_BoxCox(x1) %>% prep()
    Condition
      Warning:
      Non-positive values in selected variable.
      Warning:
      No Box-Cox transformation could be estimated for: `x1`
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Box-Cox transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Box-Cox transformation on: x1, x2, x3, x4

---

    Code
      prep(rec)
    Condition
      Warning:
      Non-positive values in selected variable.
      Warning:
      Fewer than `num_unique` values in selected variable.
      Warning:
      No Box-Cox transformation could be estimated for: `x2`, `x3`
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Box-Cox transformation on: x1, x4 | Trained

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
      * Box-Cox transformation on: <none>

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
      * Box-Cox transformation on: <none> | Trained

