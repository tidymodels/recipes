# No ICA comps

    Code
      print(ica_extract_trained)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Centering and scaling for: carbon, hydrogen, oxygen, nitrogen, ... | Trained
      * No ICA components were extracted from: carbon, hydrogen, ... | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_ica()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  IC1

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
      * ICA extraction with: <none>

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
      * ICA extraction with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      'keep_original_cols' was added to `step_ica()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * Centering and scaling for: all_predictors()
      * ICA extraction with: carbon, hydrogen

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Centering and scaling for: carbon, hydrogen, oxygen, nitrogen, ... | Trained
      * ICA extraction with: carbon, hydrogen | Trained

