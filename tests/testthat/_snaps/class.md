# bake_check_class helper function gives expected output

    Code
      bake_check_class_core(x1, "character", "x1")
    Condition
      Error in `bake_check_class_core()`:
      ! x1 should have the class(es) character but has the class(es) numeric.

---

    Code
      bake_check_class_core(x2, c("POSIXct", "Julian"), "x2")
    Condition
      Error in `bake_check_class_core()`:
      ! x2 should have the class(es) POSIXct, Julian but has the class(es) POSIXct, POSIXt.

---

    Code
      bake_check_class_core(x2, "POSIXct", "x2")
    Condition
      Error in `bake_check_class_core()`:
      ! x2 has the class(es) POSIXct, POSIXt, but only the following is/are asked POSIXct, allow_additional is FALSE.

# check_class works when class is learned

    Code
      bake(rec1, x_newdata)
    Condition
      Error:
      ! x1 should have the class(es) numeric but has the class(es) character.

---

    Code
      bake(rec1, x_newdata_2)
    Condition
      Error:
      ! x2 has the class(es) POSIXct, POSIXt, Julian, but only the following is/are asked POSIXct, POSIXt, allow_additional is FALSE.

# check_class works when class is provided

    Code
      bake(rec2, x_newdata)
    Condition
      Error:
      ! x1 should have the class(es) numeric but has the class(es) character.

---

    Code
      bake(rec3, x_newdata_2)
    Condition
      Error:
      ! x2 has the class(es) POSIXct, POSIXt, Julian, but only the following is/are asked POSIXct, POSIXt, allow_additional is FALSE.

# characters are handled correctly

    Code
      bake(rec6_NULL, sacr_fac[11:20, ])
    Condition
      Error:
      ! city should have the class(es) factor but has the class(es) character.

---

    Code
      bake(rec6_man, sacr_fac[11:20, ])
    Condition
      Error:
      ! type should have the class(es) factor but has the class(es) character.

# printing

    Code
      print(rec7)
    Output
      Recipe
      
      Inputs:
      
        2 variables (no declared roles)
      
      Operations:
      
      Checking the class(es) for everything()

---

    Code
      prep(rec7)
    Output
      Recipe
      
      Inputs:
      
        2 variables (no declared roles)
      
      Training data contained 3 data points and no missing data.
      
      Operations:
      
      Checking the class(es) for x1, x2 [trained]

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
      
      Checking the class(es) for <none>

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
      
      Checking the class(es) for <none> [trained]

