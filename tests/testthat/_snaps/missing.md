# check_missing throws error on all types

    Code
      tst(a)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contains missing values: a.

---

    Code
      tst(b)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contains missing values: b.

---

    Code
      tst(d)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contains missing values: d.

---

    Code
      tst(e)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contains missing values: e.

# check_missing works on multiple columns simultaneously

    Code
      tst(a, e)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contains missing values: a and e.

---

    Code
      tst(all_predictors())
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contains missing values: a, b, d, and e.

# check_missing on a new set

    Code
      bake(rp, na)
    Condition
      Error in `bake()`:
      ! The following columns contains missing values: a.

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
      * Check missing values for: <none>

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
      * Check missing values for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 11
      
      -- Operations 
      * Check missing values for: all_numeric()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      undeclared role: 11
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Check missing values for: mpg, cyl, disp, hp, drat, wt, ... | Trained

