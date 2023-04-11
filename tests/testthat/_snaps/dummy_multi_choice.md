# dummy variables with non-factor inputs

    Code
      prep(dummy)
    Condition
      Error in `step_dummy_multi_choice()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be nominal, or logical.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_dummy_multi_choice()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  Species_setosa

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Multi-choice dummy variables from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 4 data points and 4 incomplete rows.
      
      -- Operations 
      * Multi-choice dummy variables from: lang_1, lang_2, lang_3, lang_4 | Trained

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Multi-choice dummy variables from: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Multi-choice dummy variables from: <none> | Trained

