# bad spec

    Code
      prep(rec3, training = ex_dat, verbose = FALSE)
    Condition
      Error in `prep()`:
      ! Ordinal factor variables should be selected as inputs into this step.

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

