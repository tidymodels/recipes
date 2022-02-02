# in recipe

    Code
      bake(rec2, test)
    Error <rlang_error>
      min x is -10, lower bound is -5, max x is 110, upper bound is 105

---

    Code
      bake(rec3, test)
    Warning <rlang_warning>
      min x is -10, lower bound is -5, max x is 110, upper bound is 105
      min y is -10, lower bound is -2.5, max x is 60, upper bound is 52.5
    Output
      # A tibble: 2 x 2
            x     y
        <dbl> <dbl>
      1   -10   -10
      2   110    60

---

    Code
      bake(rec4, test)
    Error <rlang_error>
      max y is 60, upper bound is 55

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
      
      Checking range of <none>

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
      
      Checking range of <none> [trained]

