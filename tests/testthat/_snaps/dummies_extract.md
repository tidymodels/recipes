# error when neither sep or pattern is specified

    Code
      recipe(~medium, data = tate_text) %>% step_dummy_extract(medium) %>% prep()
    Condition
      Error in `dummy_extract()`:
      ! `sep` or `pattern` must be specified.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Extract patterns from all_predictors()

---

    Code
      prep(rec, training = tate_text, verbose = TRUE)
    Output
      oper 1 step dummy extract [training] 
      The retained training set is ~ 50.39 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 4284 data points and no missing data.
      
      Operations:
      
      Extract patterns from medium [trained]

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
      
      Extract patterns from <none>

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
      
      Extract patterns from <none> [trained]

