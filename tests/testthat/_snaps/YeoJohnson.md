# missing data

    Code
      prep(rec_false, training = ex_dat, verbose = FALSE)
    Condition
      Error:
      ! Must subset columns with a valid subscript vector.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

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
      prep(rec, training = ex_dat, verbose = TRUE)
    Output
      oper 1 step YeoJohnson [training] 
      The retained training set is ~ 0 Mb  in memory.
      
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

