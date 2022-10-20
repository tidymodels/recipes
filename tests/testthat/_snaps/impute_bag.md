# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_bagimpute()
    Condition
      Error:
      ! `step_bagimpute()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use `step_impute_bag()` instead.

# printing

    Code
      print(imputed)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          6
      
      Operations:
      
      Bagged tree imputation for carbon

---

    Code
      prep(imputed)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          6
      
      Training data contained 536 data points and no missing data.
      
      Operations:
      
      Bagged tree imputation for carbon [trained]

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
      
      Bagged tree imputation for <none>

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
      
      Bagged tree imputation for <none> [trained]

