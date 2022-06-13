# basic rename

    Code
      prep(rec, training = iris %>% slice(1:75))
    Condition
      Error in `recipes_eval_select()`:
      ! Can't rename variables in this context.

# remove with quasi-quotation

    Code
      prep(rec_1, training = iris %>% slice(1:75))
    Condition
      Error in `all_of()`:
      ! object 'sepal_vars' not found

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Variables removed x1

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
      
      Variables removed x1 [trained]

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
      
      Variables removed <none>

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
      
      Variables removed <none> [trained]

