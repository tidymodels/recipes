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
      prep(imputed, training = biomass, verbose = TRUE)
    Output
      oper 1 step impute bag [training] 
      The retained training set is ~ 0.03 Mb  in memory.
      
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

