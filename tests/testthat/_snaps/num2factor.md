# bad args

    Code
      prep(step_num2factor(rec, w, x, levels = c("one", "two")), ex_dat)
    Condition
      Error in `step_num2factor()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `w`

---

    Code
      prep(step_num2factor(rec, w, x), ex_dat)
    Condition
      Error in `step_num2factor()`:
      ! Please provide a character vector of appropriate length for `levels`.

---

    Code
      prep(step_num2factor(rec, z, levels = rev(LETTERS[1:10]), transform = 2))
    Condition
      Error in `step_num2factor()`:
      Caused by error in `prep()`:
      ! `transform` must be a function, not the number 2.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(ex_1, new_data = ex_dat[, 1:2])
    Condition
      Error in `step_num2factor()`:
      ! The following required column is missing from `new_data`: z.

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
      * Factor variables from: <none>

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
      * Factor variables from: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Factor variables from: z

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Factor variables from: z | Trained

