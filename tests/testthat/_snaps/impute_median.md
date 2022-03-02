# non-numeric

    Code
      prep(impute_rec, training = credit_tr, verbose = FALSE)
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# printing

    Code
      print(impute_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         13
      
      Operations:
      
      Median imputation for Age, Assets, Income

---

    Code
      prep(impute_rec, training = credit_tr, verbose = TRUE)
    Output
      oper 1 step impute median [training] 
      The retained training set is ~ 0.11 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         13
      
      Training data contained 2000 data points and 186 incomplete rows. 
      
      Operations:
      
      Median imputation for Age, Assets, Income [trained]

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
      
      Median imputation for <none>

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
      
      Median imputation for <none> [trained]

