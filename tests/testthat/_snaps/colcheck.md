# check_col works in the bake stage

    Code
      rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars[-1])
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "mpg".
      i These columns have one of the following roles, which are required at `bake()` time: "predictor".

---

    Code
      rp2 %>% check_cols(cyl, mpg, drat) %>% prep() %>% bake(mtcars[, c(2, 5)])
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "mpg".
      i These columns have one of the following roles, which are required at `bake()` time: "predictor".

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
      * Check if the following columns are present:: everything()

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
      * Check if the following columns are present:: mpg, disp, hp, ... | Trained

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
      * Check if the following columns are present:: <none>

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
      * Check if the following columns are present:: <none> | Trained

