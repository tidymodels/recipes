# printing

    Code
      print(holiday_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Holiday features from all_predictors()

---

    Code
      prep(holiday_rec, training = test_data, verbose = TRUE)
    Output
      oper 1 step holiday [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 365 data points and no missing data.
      
      Operations:
      
      Holiday features from day [trained]

# can prep recipes with no keep_original_cols

    Code
      holiday_rec <- prep(holiday_rec, training = test_data, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_holiday()` after this recipe was created.
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
      
      Holiday features from <none>

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
      
      Holiday features from <none> [trained]

