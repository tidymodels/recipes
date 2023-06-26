# error when neither sep or pattern is specified

    Code
      recipe(~medium, data = tate_text) %>% step_dummy_extract(medium) %>% prep()
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `prep()`:
      ! `sep` or `pattern` must be specified.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  Species_setosa

# case weights

    Code
      dummy_prepped
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Extract patterns from: medium | Trained, weighted

---

    Code
      dummy_prepped
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Extract patterns from: medium | Trained, ignored weights

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
      * Extract patterns from: <none>

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
      * Extract patterns from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      'keep_original_cols' was added to `step_dummy_extract()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Extract patterns from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4284 data points and no incomplete rows.
      
      -- Operations 
      * Extract patterns from: medium | Trained

