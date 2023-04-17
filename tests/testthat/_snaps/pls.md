# PLS-DA, dense loadings, multiple outcomes

    Code
      prep(rec)
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `step_pls()` only supports multivariate models for numeric outcomes.

# PLS-DA, sparse loadings, multiple outcomes

    Code
      prep(rec)
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `step_pls()` only supports multivariate models for numeric outcomes.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pls()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  PLS1

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_pls(outcome = "mpg", preserve = TRUE)
    Condition
      Error:
      ! The `preserve` argument of `step_pls()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use the `keep_original_cols` argument instead.

# print method

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * PLS feature extraction with: all_predictors()

---

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * PLS feature extraction with: carbon, hydrogen, oxygen, ... | Trained

# can prep recipes with no keep_original_cols

    Code
      pls_trained <- prep(pls_rec, training = biom_tr, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_pls()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

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
      * PLS feature extraction with: <none>

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
      * PLS feature extraction with: <none> | Trained

