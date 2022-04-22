# 'other' already in use

    Code
      prep(others, training = okc_tr, strings_as_factors = FALSE)
    Condition
      Error in `FUN()`:
      ! The level other is already a factor level that will be retained. Please choose a different value.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Collapsing factor levels for diet, location

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 59655 data points and 24288 incomplete rows. 
      
      Operations:
      
      Collapsing factor levels for diet, location [trained]

# if the threshold argument is greather than one then it should be an integer(ish)

    Code
      rec %>% step_other(diet, location, threshold = 3.14)
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

