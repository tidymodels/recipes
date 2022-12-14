# dummy variables with non-factor inputs

    Code
      prep(dummy)
    Condition
      Error in `step_dummy_multi_choice()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be nominal, or logical.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Operations:
      
      Multi-choice dummy variables from all_predictors()

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Training data contained 4 data points and 4 incomplete rows. 
      
      Operations:
      
      Multi-choice dummy variables from lang_1, lang_2, lang_3, lang_4 [trained]

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
      
      Multi-choice dummy variables from <none>

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
      
      Multi-choice dummy variables from <none> [trained]

