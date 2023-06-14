# new_values_func breaks when x contains new values

    Code
      new_values_func(x, allowed_values[-3], colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! MacGyver contains the new value(s): c

# new_values_func correctly prints multiple new values

    Code
      new_values_func(x, allowed_values[-c(2:3)], colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! MacGyver contains the new value(s): b,c

# new_values_func breaks when NA is new value and ignore_NA is FALSE

    Code
      new_values_func(x_na, allowed_values, ignore_NA = FALSE, colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! MacGyver contains the new value(s): NA

# new_values_func correctly prints multiple new values with NA

    Code
      new_values_func(x_na, allowed_values[-3], ignore_NA = FALSE, colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! MacGyver contains the new value(s): c,NA

# new_values_func correctly prints only non na-values when also NA as new value and ignore_NA is TRUE

    Code
      new_values_func(x_na, allowed_values[-3], ignore_NA = TRUE, colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! MacGyver contains the new value(s): c

# check_new_values breaks with new values

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2[1:4, , drop = FALSE])
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): d

---

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): d,e

# check_new_values ignores NA by default

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): d

# check_new_values not ignoring NA argument

    Code
      recipe(x1) %>% check_new_values(a, ignore_NA = FALSE) %>% prep() %>% bake(x2[-4,
        , drop = FALSE])
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): NA

---

    Code
      recipe(x1) %>% check_new_values(a, ignore_NA = FALSE) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): d,NA

# check_new_values works on doubles

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): 1.3

# check_new_values works on integers

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): 3

# check_new_values works on factors

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): c

# check_new_values works on characters

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): c

# check_new_values works on logicals

    Code
      recipe(x1) %>% check_new_values(a) %>% prep() %>% bake(x2)
    Condition
      Error in `new_values_func()`:
      ! a contains the new value(s): FALSE

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
      * Checking no new_values for: <none>

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
      * Checking no new_values for: <none> | Trained

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
      * Checking no new_values for: disp

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
      * Checking no new_values for: disp | Trained

