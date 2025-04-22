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
      * Centering and scaling for: carbon, hydrogen, oxygen, ... | Trained
      * No ICA components were extracted from: carbon hydrogen, ... | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_ica()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `IC1`

# rethrows error correctly from implementation

    Code
      prep(step_ica(recipe(~., data = mtcars), all_predictors()))
    Condition
      Error in `step_ica()`:
      Caused by error in `prep()`:
      ! Failed to compute:
      Caused by error in `fastICA::fastICA()`:
      ! mocked error

# check_options() is used

    Code
      prep(step_ica(recipe(~., data = mtcars), all_predictors(), options = TRUE))
    Condition
      Error in `step_ica()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(ica_extract_trained, new_data = biomass_tr[, c(-3)])
    Condition
      Error in `step_ica()`:
      ! The following required column is missing from `new_data`: carbon.

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
      `keep_original_cols` was added to `step_ica()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

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
      * ICA extraction with: carbon hydrogen

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
      * Centering and scaling for: carbon, hydrogen, oxygen, ... | Trained
      * ICA extraction with: carbon hydrogen | Trained

# bad args

    Code
      prep(step_ica(rec, carbon, hydrogen, prefix = 2))
    Condition
      Error in `step_ica()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 2.

