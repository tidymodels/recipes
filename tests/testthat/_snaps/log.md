# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Operations:
      
      Log transformation on x1, x2, x3, x4

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Training data contained 20 data points and no missing data.
      
      Operations:
      
      Log transformation on x1, x2, x3, x4 [trained]

# signed arg

    Code
      prep(rec2)
    Condition
      Warning:
      When signed is TRUE, offset will be ignored
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 4 data points and no missing data.
      
      Operations:
      
      Signed log transformation on x [trained]

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
      
      Log transformation on <none>

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
      
      Log transformation on <none> [trained]

