# check_missing throws error on all types

    Code
      tst(a)
    Condition
      Error in `bake()`:
      ! The following columns contain missing values: `a`.

---

    Code
      tst(b)
    Condition
      Error in `bake()`:
      ! The following columns contain missing values: `b`.

---

    Code
      tst(d)
    Condition
      Error in `bake()`:
      ! The following columns contain missing values: `d`.

---

    Code
      tst(e)
    Condition
      Error in `bake()`:
      ! The following columns contain missing values: `e`.

# check_missing works on multiple columns simultaneously

    Code
      tst(a, e)
    Condition
      Error in `bake()`:
      ! The following columns contain missing values: `a`, `e`.

---

    Code
      tst(everything())
    Condition
      Error in `bake()`:
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
    Output
      Recipe
      
      Inputs:
      
        11 variables (no declared roles)
      
      Operations:
      
      Check missing values for all_numeric()

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
        11 variables (no declared roles)
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Check missing values for mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gea... [trained]

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Check missing values for <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Check missing values for <none> [trained]

