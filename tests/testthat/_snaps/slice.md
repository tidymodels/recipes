# quasiquotation

    Code
      prep(rec_1, training = iris %>% slice(1:75))
    Condition
      Error in `dplyr::slice()`:
      ! Problem while evaluating `..1 = values`.
      Caused by error:
      ! object 'values' not found

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Row filtering via position 1:2

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Row filtering via position 1:2 [trained]

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
      
      Row filtering via position <none>

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
      
      Row filtering via position <none> [trained]

