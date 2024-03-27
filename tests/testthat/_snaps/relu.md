# input checking

    Code
      recipe(~., data = df) %>% step_relu(val1, shift = TRUE) %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      ! `shift` must be a number, not `TRUE`.

---

    Code
      recipe(~., data = df) %>% step_relu(val1, reverse = 3) %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      ! `reverse` must be `TRUE` or `FALSE`, not the number 3.

---

    Code
      recipe(~., data = df) %>% step_relu(val1, smooth = "cat") %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      ! `smooth` must be `TRUE` or `FALSE`, not the string "cat".

---

    Code
      recipe(~., data = df) %>% step_relu(val2) %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `val2`

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
      * Adding relu transform for: <none>

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
      * Adding relu transform for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Operations 
      * Adding relu transform for: disp

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Adding relu transform for: disp | Trained

