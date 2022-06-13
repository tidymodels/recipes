# printing

    Code
      print(date_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Date features from all_predictors()

---

    Code
      prep(date_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 10 data points and no missing data.
      
      Operations:
      
      Date features from Dan, Stefan [trained]

# can prep recipes with no keep_original_cols

    Code
      date_rec <- prep(date_rec, training = examples, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_date()` after this recipe was created.
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
      
      Date features from <none>

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
      
      Date features from <none> [trained]

