# quasiquotation

    Code
      prep(rec_1, training = iris %>% slice(1:75))
    Condition
      Error in `dplyr::mutate()`:
      ! Problem while computing `new_var = Sepal.Width * const`.
      Caused by error:
      ! object 'const' not found

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Variable mutation for 5

---

    Code
      prep(rec, training = iris, verbose = TRUE)
    Output
      oper 1 step mutate [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Variable mutation for ~5 [trained]

---

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Variable mutation for contains("Sepal")

---

    Code
      prep(rec, training = iris, verbose = TRUE)
    Output
      oper 1 step mutate at [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Variable mutation for Sepal.Length, Sepal.Width [trained]

# mutate_at - empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Variable mutation for <none>

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
      
      Variable mutation for <none> [trained]

