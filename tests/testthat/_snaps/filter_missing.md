# printing

    Code
      print(filtering)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Operations:
      
      Missing value column filter on all_predictors()

---

    Code
      prep(filtering, training = dat, verbose = TRUE)
    Output
      oper 1 step filter missing [training] 
      The retained training set is ~ 0 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Training data contained 100 data points and 100 incomplete rows. 
      
      Operations:
      
      Missing value column filter removed dbl4, dbl5, chr2 [trained]

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
      
      Missing value column filter on <none>

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
      
      Missing value column filter removed <none> [trained]

