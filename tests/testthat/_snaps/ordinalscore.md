# bad spec

    Code
      prep(rec3, training = ex_dat, verbose = FALSE)
    Condition
      Error in `step_ordinalscore()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be ordered.

# printing

    Code
      print(rec5)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Scoring for starts_with("ord")

---

    Code
      prep(rec5)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 20 data points and no missing data.
      
      Operations:
      
      Scoring for ord1, ord2, ord3 [trained]

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
      
      Scoring for <none>

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
      
      Scoring for <none> [trained]

