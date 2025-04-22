# bad spec

    Code
      prep(rec3, training = ex_dat, verbose = FALSE)
    Condition
      Error in `step_ordinalscore()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be ordered.
      * 1 double variable found: `numbers`
      * 1 factor variable found: `fact`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec1, new_data = ex_dat[, 1:3])
    Condition
      Error in `step_ordinalscore()`:
      ! The following required columns are missing from `new_data`: ord2 and ord3.

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
      * Scoring for: <none>

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
      * Scoring for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Scoring for: starts_with("ord")

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
      * Scoring for: ord1, ord2, ord3 | Trained

# bad args

    Code
      prep(step_ordinalscore(recipe(~., data = ex_dat), starts_with("ord"), convert = NULL))
    Condition
      Error in `step_ordinalscore()`:
      Caused by error in `prep()`:
      ! `convert` must be a function, not `NULL`.

