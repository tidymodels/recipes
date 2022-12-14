# 'other' already in use

    Code
      prep(others, training = sacr_tr_chr, strings_as_factors = FALSE)
    Condition
      Error in `step_other()`:
      Caused by error in `prep()`:
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
      
      Collapsing factor levels for city, zip

---

    Code
      prep(rec)
    Output
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

# othering with case weights

    Code
      others
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          1
      
      Training data contained 732 data points and no missing data.
      
      Operations:
      
      Collapsing factor levels for city [weighted, trained]

---

    Code
      others
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          1
      
      Training data contained 732 data points and no missing data.
      
      Operations:
      
      Collapsing factor levels for city [ignored weights, trained]

