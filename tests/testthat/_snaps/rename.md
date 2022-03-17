# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Variable renaming for wat

---

    Code
      prep(rec, training = iris, verbose = TRUE)
    Output
      oper 1 step rename [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Variable renaming for wat [trained]

---

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Variable renaming for contains("Sepal")

---

    Code
      prep(rec, training = iris, verbose = TRUE)
    Output
      oper 1 step rename at [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Variable renaming for Sepal.Length, Sepal.Width [trained]

# rename - empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Variable renaming for <none>

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
      
      Variable renaming for <none> [trained]

# mulitple functions

    Code
      prep(rec, training = iris %>% slice(1:75))
    Condition
      Error in `dplyr::rename_at()`:
      ! `.funs` must contain one renaming function, not 2.

# rename_at - empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Variable renaming for <none>

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
      
      Variable renaming for <none> [trained]

