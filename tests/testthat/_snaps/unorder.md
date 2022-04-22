# wrong vars

    Code
      prep(rec2, training = examples, verbose = FALSE)
    Condition
      Warning:
      `step_unorder` requires ordered factors. Variables X1 will be ignored.
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 12 data points and no missing data.
      
      Operations:
      
      Unordered variables X2 [trained]

---

    Code
      prep(rec3, training = examples, verbose = FALSE)
    Condition
      Warning:
      `step_unorder` requires ordered factors. Variables X1 will be ignored.
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 12 data points and no missing data.
      
      Operations:
      
      Unordered variables <none> [trained]

# printing

    Code
      print(rec4)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Unordered variables X2

---

    Code
      prep(rec4)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 12 data points and no missing data.
      
      Operations:
      
      Unordered variables X2 [trained]

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
      
      Unordered variables <none>

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
      
      Unordered variables <none> [trained]

