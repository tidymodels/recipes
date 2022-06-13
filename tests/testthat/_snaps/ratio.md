# wrong type

    Code
      prep(rec4, ex_dat, verbose = FALSE)
    Condition
      Error in `prep()`:
      ! The ratio variables should be numeric

---

    Code
      prep(rec5, ex_dat, verbose = FALSE)
    Condition
      Error in `prep()`:
      ! The ratio variables should be numeric

---

    Code
      prep(rec6, ex_dat, verbose = FALSE)
    Condition
      Error in `prep()`:
      ! The ratio variables should be numeric

# printing

    Code
      print(rec3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Ratios from all_numeric(), all_numeric()

---

    Code
      prep(rec3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 10 data points and 1 incomplete row. 
      
      Operations:
      
      Ratios from x2, x3, x4, x1, x1, x2, x3, x4 [trained]

# can prep recipes with no keep_original_cols

    Code
      prep1 <- prep(rec1, training = ex_dat, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_ratio()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

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
      
      Ratios from mpg

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
      
      Ratios from <none> [trained]

