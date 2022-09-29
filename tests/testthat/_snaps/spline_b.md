# printing

    Code
      print(with_ns)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Operations:
      
      Basis spline expansion carbon, hydrogen

---

    Code
      prep(with_ns)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Basis spline expansion carbon, hydrogen [trained]

---

    Code
      rec %>% step_spline_b(carbon, hydrogen, deg_free = -1) %>% prep()
    Condition
      Warning:
      The 'df' must be a nonnegative integer.
      Warning:
      The 'df' must be a nonnegative integer.
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Basis spline expansion <none> [trained]

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
      
      Basis spline expansion <none>

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
      
      Basis spline expansion <none> [trained]

