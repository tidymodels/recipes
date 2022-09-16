# out of bounds logit trans

    Code
      prep(rec, training = ex_dat, verbose = FALSE)
    Condition
      Error in `binomial()$linkfun()`:
      ! Value -0.77772 out of range (0, 1)

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Operations:
      
      Logit transformation on x1

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 20 data points and no missing data.
      
      Operations:
      
      Logit transformation on x1 [trained]

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
      
      Logit transformation on <none>

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
      
      Logit transformation on <none> [trained]

