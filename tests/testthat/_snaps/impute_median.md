# non-numeric

    Code
      prep(impute_rec, training = credit_tr, verbose = FALSE)
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_medianimpute()
    Condition
      Error:
      ! `step_medianimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_median()` instead.

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
      prep(impute_rec)
    Output
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

# case weights

    Code
      impute_rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor         12
      
      Training data contained 2000 data points and 186 incomplete rows. 
      
      Operations:
      
      Median imputation for Age, Assets, Income [weighted, trained]

---

    Code
      impute_rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor         12
      
      Training data contained 2000 data points and 186 incomplete rows. 
      
      Operations:
      
      Median imputation for Age, Assets, Income [ignored weights, trained]

