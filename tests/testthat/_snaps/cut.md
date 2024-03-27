# step_cut throws error on non-numerics

    Code
      recipe(x) %>% step_cut(cat_var, breaks = 2) %>% prep()
    Condition
      Error in `step_cut()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `cat_var`

---

    Code
      recipe(x) %>% step_cut(everything(), breaks = 2) %>% prep()
    Condition
      Error in `step_cut()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `cat_var`

# full_breaks_check will give warnings

    Code
      full_breaks_check(10)
    Condition
      Error:
      ! Variable is invariant and equal to break point.

---

    Code
      full_breaks_check(c(10, 20))
    Condition
      Warning:
      This will create a factor with one value only.

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
      * Cut numeric for: <none>

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
      * Cut numeric for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Cut numeric for: disp

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Cut numeric for: disp | Trained

