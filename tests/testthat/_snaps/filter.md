# quasiquotation

    Code
      prep(rec_1, training = iris %>% slice(1:75))
    Condition
      Error in `step_filter()`:
      Caused by error in `dplyr::filter()`:
      ! Problem while computing `..2 = Species %in% values`.
      Caused by error in `Species %in% values`:
      ! object 'values' not found

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Row filtering using Sepal.Length > 4.5

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Row filtering using ~Sepal.Length > 4.5 [trained]

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
      
      Row filtering using <none>

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
      
      Row filtering using <none> [trained]

