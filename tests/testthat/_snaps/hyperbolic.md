# wrong arguments

    Code
      prep(step_hyperbolic(rec, func = "cos"))
    Condition
      Error in `step_hyperbolic()`:
      ! `func` must be one of "sinh", "cosh", or "tanh", not "cos".
      i Did you mean "cosh"?

---

    Code
      prep(step_hyperbolic(rec, inverse = 2))
    Condition
      Error in `step_hyperbolic()`:
      Caused by error in `prep()`:
      ! `x$inverse` must be `TRUE` or `FALSE`, not the number 2.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, 2, drop = FALSE])
    Condition
      Error in `step_hyperbolic()`:
      ! The following required column is missing from `new_data`: x1.

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
      * Hyperbolic sin (inv) transformation on: <none>

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
      * Hyperbolic sin (inv) transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Hyperbolic sin (inv) transformation on: x1 x2

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
      * Hyperbolic sin (inv) transformation on: x1 x2 | Trained

