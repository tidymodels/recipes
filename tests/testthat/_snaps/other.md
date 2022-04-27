# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Collapsing factor levels for city, zip

---

    Code
      prep(rec, training = sacr_tr, verbose = TRUE)
    Output
      oper 1 step other [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 732 data points and no missing data.
      
      Operations:
      
      Collapsing factor levels for city, zip [trained]

# if the threshold argument is greather than one then it should be an integer(ish)

    Code
      rec %>% step_other(city, zip, threshold = 3.14)
    Condition
      Error in `step_other()`:
      ! If `threshold` is greater than one it should be an integer.

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
      
      Collapsing factor levels for <none>

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
      
      Collapsing factor levels for <none> [trained]

