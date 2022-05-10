# printing

    Code
      print(rec3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Operations:
      
      Shuffled everything()

---

    Code
      prep(rec3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Training data contained 50 data points and no missing data.
      
      Operations:
      
      Shuffled x1, x2, x3, x4, y [trained]

# bake a single row

    Code
      dat4 <- bake(rec4, dat[1, ], everything())
    Condition
      Warning:
      `new_data` contains a single row; unable to shuffle

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
      
      Shuffled <none>

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
      
      Shuffled <none> [trained]

