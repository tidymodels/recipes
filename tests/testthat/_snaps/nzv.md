# altered freq_cut and unique_cut

    The `options` argument of `step_nzv()` was deprecated in recipes 0.1.7 and is now defunct.
    Please use the arguments `freq_cut` and `unique_cut` instead.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Operations:
      
      Sparse, unbalanced variable filter on x1, x2, x3, x4

---

    Code
      prep(rec, training = dat, verbose = TRUE)
    Output
      oper 1 step nzv [training] 
      The retained training set is ~ 0 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Training data contained 50 data points and no missing data.
      
      Operations:
      
      Sparse, unbalanced variable filter removed x3, x4 [trained]

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
      
      Sparse, unbalanced variable filter on <none>

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
      
      Sparse, unbalanced variable filter removed <none> [trained]

