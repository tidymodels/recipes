# check_missing throws error on all types

    Code
      tst(a)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contain missing values: `a`.

---

    Code
      tst(b)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contain missing values: `b`.

---

    Code
      tst(d)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contain missing values: `d`.

---

    Code
      tst(e)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contain missing values: `e`.

# check_missing works on multiple columns simultaneously

    Code
      tst(a, e)
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contain missing values: `a`, `e`.

---

    Code
      tst(everything())
    Condition
      Error in `check_missing()`:
      Caused by error in `bake()`:
      ! The following columns contain missing values: `a`, `b`, `d`, `e`.

# check_missing on a new set

    Code
      bake(rp, na)
    Condition
      Error in `bake()`:
      ! The following columns contain missing values: `a`.

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
      * Check missing values for: mpg, cyl, disp, hp, drat, wt, qsec, ... | Trained

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

