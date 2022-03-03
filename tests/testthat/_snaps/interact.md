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
      prep(int_rec, training = dat_tr, verbose = TRUE)
    Output
      oper 1 step interact [training] 
      The retained training set is ~ 0 Mb  in memory.
      
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

