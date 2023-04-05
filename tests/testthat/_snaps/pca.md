# correct PCA values

    Code
      tidy(pca_extract_trained, number = 3, type = "variances")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "coef", "variance"

# printing

    Code
      print(pca_extract)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * PCA extraction with: carbon, hydrogen, oxygen, nitrogen, sulfur

---

    Code
      prep(pca_extract)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * PCA extraction with: carbon, hydrogen, oxygen, nitrogen, sulfur | Trained

# No PCA comps

    Code
      print(pca_extract_trained)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * No PCA components were extracted from: carbon, hydrogen, ... | Trained

# backwards compatible with 0.1.17

    Code
      pca_extract
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Centering for: carbon, hydrogen, oxygen, nitrogen, sulfur | Trained
      * Scaling for: carbon, hydrogen, oxygen, nitrogen, sulfur | Trained
      * PCA extraction with: carbon, hydrogen, oxygen, nitrogen, sulfur | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pca()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  PC1

# can prep recipes with no keep_original_cols

    Code
      pca_extract_trained <- prep(pca_extract, training = biomass_tr, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_pca()` after this recipe was created.
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
      * PCA extraction with: <none>

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
      * No PCA components were extracted from: <none> | Trained

# case weights

    Code
      pca_extract_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * PCA extraction with: carbon, hydrogen, oxygen, sulfur | Trained, weighted

---

    Code
      pca_extract_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * PCA extraction with: carbon, hydrogen, oxygen, ... | Trained, ignored weights

