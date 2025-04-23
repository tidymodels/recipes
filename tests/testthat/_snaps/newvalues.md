# new_values_func breaks when x contains new values

    Code
      new_values_func(x, allowed_values[-3], colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! `MacGyver` contains the new value: "c".

# new_values_func correctly prints multiple new values

    Code
      new_values_func(x, allowed_values[-c(2:3)], colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! `MacGyver` contains the new values: "b" and "c".

# new_values_func breaks when NA is new value and ignore_NA is FALSE

    Code
      new_values_func(x_na, allowed_values, ignore_NA = FALSE, colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! `MacGyver` contains the new value: NA.

# new_values_func correctly prints multiple new values with NA

    Code
      new_values_func(x_na, allowed_values[-3], ignore_NA = FALSE, colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! `MacGyver` contains the new values: "c" and NA.

# new_values_func correctly prints only non na-values when also NA as new value and ignore_NA is TRUE

    Code
      new_values_func(x_na, allowed_values[-3], ignore_NA = TRUE, colname = "MacGyver")
    Condition
      Error in `new_values_func()`:
      ! `MacGyver` contains the new value: "c".

# check_new_values breaks with new values

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2[1:4, , drop = FALSE])
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: "d".

---

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new values: "d" and "e".

# check_new_values ignores NA by default

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: "d".

# check_new_values not ignoring NA argument

    Code
      bake(prep(check_new_values(recipe(x1), a, ignore_NA = FALSE)), x2[-4, , drop = FALSE])
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: NA.

---

    Code
      bake(prep(check_new_values(recipe(x1), a, ignore_NA = FALSE)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new values: "d" and NA.

# check_new_values works on doubles

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: "1.3".

# check_new_values works on integers

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: "3".

# check_new_values works on factors

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: "c".

# check_new_values works on characters

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: "c".

# check_new_values works on logicals

    Code
      bake(prep(check_new_values(recipe(x1), a)), x2)
    Condition
      Error in `new_values_func()`:
      ! `a` contains the new value: "FALSE".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mtcars[, -3])
    Condition
      Error in `check_new_values()`:
      ! The following required column is missing from `new_data`: disp.

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

# bad args

    Code
      prep(check_new_values(recipe(mpg ~ ., mtcars), disp, ignore_NA = 2))
    Condition
      Error in `check_new_values()`:
      Caused by error in `prep()`:
      ! `ignore_NA` must be `TRUE` or `FALSE`, not the number 2.

