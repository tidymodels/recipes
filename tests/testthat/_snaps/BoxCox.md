# simple Box Cox

    Code
      rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    Warning <warning>
      Non-positive values in selected variable.
      Fewer than `num_unique` values in selected variable.
      No Box-Cox transformation could be estimated for: `x2`, `x3`

# printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Operations:
      
      Box-Cox transformation on x1, x2, x3, x4

---

    Code
      prep(rec, training = ex_dat)
    Warning <warning>
      Non-positive values in selected variable.
      Fewer than `num_unique` values in selected variable.
      No Box-Cox transformation could be estimated for: `x2`, `x3`
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Training data contained 20 data points and no missing data.
      
      Operations:
      
      Box-Cox transformation on x1, x4 [trained]

