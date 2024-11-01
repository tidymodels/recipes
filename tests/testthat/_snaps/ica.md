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
      * No ICA components were extracted from: carbon and hydrogen, ... | Trained

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
      recipe(~., data = mtcars) %>% step_ica(all_predictors()) %>% prep()
    Condition
      Error in `step_ica()`:
      Caused by error in `prep()`:
      x Failed with error:
      i Error in fastICA::fastICA(n.comp = 5, X = as.matrix(training[, col_names]), : mocked error

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
      * ICA extraction with: carbon and hydrogen

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
      * ICA extraction with: carbon and hydrogen | Trained

# bad args

    Code
      rec %>% step_ica(carbon, hydrogen, prefix = 2) %>% prep()
    Condition
      Error in `step_ica()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 2.

