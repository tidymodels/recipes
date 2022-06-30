# printing

    Code
      print(int_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          6
      
      Operations:
      
      Interactions with x1:x2

---

    Code
      prep(int_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          6
      
      Training data contained 10 data points and no missing data.
      
      Operations:
      
      Interactions with x1:x2 [trained]

# missing columns

    Code
      no_fail_rec <- prep(no_fail, dat_tr)
    Condition
      Warning:
      Interaction specification failed for: ~x1:x2. No interactions will be created.

---

    Code
      one_int_rec <- prep(one_int, dat_tr)
    Condition
      Warning:
      Interaction specification failed for: ~x1:x2. No interactions will be created.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(int_rec_trained, dat_tr[, 4:6])
    Condition
      Error in `step_interact()`:
      ! The following required columns are missing from `new_data` in step '': z and x1.

