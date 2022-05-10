# missing data

    Code
      prep(rec_false, training = ex_dat, verbose = FALSE)
    Condition
      Error in `FUN()`:
      ! Missing values are not allowed for the YJ transformation. See `na_rm` option

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Operations:
      
      Yeo-Johnson transformation on x1, x2, x3, x4

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
      
      Yeo-Johnson transformation on x1, x2, x4 [trained]

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
      
      Yeo-Johnson transformation on <none>

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
      
      Yeo-Johnson transformation on <none> [trained]

