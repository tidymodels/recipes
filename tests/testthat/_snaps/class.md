# bake_check_class helper function gives expected output

    Code
      bake_check_class_core(x1, "character", "x1")
    Condition
      Error in `bake_check_class_core()`:
      ! `x1` should have the class <character> but has the class <numeric>.

---

    Code
      bake_check_class_core(x2, c("POSIXct", "Julian"), "x2")
    Condition
      Error in `bake_check_class_core()`:
      ! `x2` should have the class <POSIXct/Julian> but has the classes <POSIXct/POSIXt>.

---

    Code
      bake_check_class_core(x2, "POSIXct", "x2")
    Condition
      Error in `bake_check_class_core()`:
      x `x2` has class <POSIXct/POSIXt> but only the following are asked: <POSIXct>.
      i This error is shown because `allow_additional` is set to "FALSE".

# check_class works when class is learned

    Code
      bake(rec1, x_newdata)
    Condition
      Error:
      ! `x1` should have the class <numeric> but has the class <character>.

---

    Code
      bake(rec1, x_newdata_2)
    Condition
      Error:
      x `x2` has class <POSIXct/POSIXt/Julian> but only the following are asked: <POSIXct/POSIXt>.
      i This error is shown because `allow_additional` is set to "FALSE".

# check_class works when class is provided

    Code
      bake(rec2, x_newdata)
    Condition
      Error:
      ! `x1` should have the class <numeric> but has the class <character>.

---

    Code
      bake(rec3, x_newdata_2)
    Condition
      Error:
      x `x2` has class <POSIXct/POSIXt/Julian> but only the following are asked: <POSIXct/POSIXt>.
      i This error is shown because `allow_additional` is set to "FALSE".

# characters are handled correctly

    Code
      bake(rec6_NULL, sacr_fac[11:20, ])
    Condition
      Error:
      ! `city` should have the class <factor> but has the class <character>.

---

    Code
      bake(rec6_man, sacr_fac[11:20, ])
    Condition
      Error:
      ! `type` should have the class <factor> but has the class <character>.

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
      * Checking the class(es) for: <none>

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
      * Checking the class(es) for: <none> | Trained

# printing

    Code
      print(rec7)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Checking the class(es) for: everything()

---

    Code
      prep(rec7)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Checking the class(es) for: cyl, disp, hp, drat, wt, qsec, ... | Trained

