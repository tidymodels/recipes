# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Hyperbolic sin (inv) transformation on x1, x2

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 20 data points and no missing data.
      
      Operations:
      
      Hyperbolic sin (inv) transformation on x1, x2 [trained]

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
      
      Hyperbolic sin (inv) transformation on <none>

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
      
      Hyperbolic sin (inv) transformation on <none> [trained]

# wrong function

    `func` must be one of "sinh", "cosh", or "tanh", not "cos".
    i Did you mean "cosh"?

