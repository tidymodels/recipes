# correct PCA values

    Code
      tidy(pca_extract_trained, number = 3, type = "variances")
    Condition
      Error in `tidy()`:
      ! `type` must be one of "coef" or "variance", not "variances".
      i Did you mean "variance"?

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
      * No PCA components were extracted from: carbon hydrogen, ... | Trained

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
      ! Name collision occurred. The following variable names already exist:
      * `PC1`

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
      * PCA extraction with: carbon hydrogen, ... | Trained, ignored weights

# check_options() is used

    Code
      prep(step_pca(recipe(~mpg, data = mtcars), mpg, options = TRUE))
    Condition
      Error in `step_pca()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(pca_extract_trained, new_data = biomass_te[, c(-3)])
    Condition
      Error in `step_pca()`:
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

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_pca()` after this recipe was created.
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
      * PCA extraction with: carbon, hydrogen, oxygen, nitrogen, sulfur

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
      * PCA extraction with: carbon, hydrogen, oxygen, nitrogen, sulfur | Trained

# bad args

    Code
      prep(step_pca(recipe(~., data = mtcars), all_numeric_predictors(), num_comp = -
      1))
    Condition
      Error in `step_pca()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      prep(step_pca(recipe(~., data = mtcars), all_numeric_predictors(), prefix = 1))
    Condition
      Error in `step_pca()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 1.

---

    Code
      prep(step_pca(recipe(~., data = mtcars), all_numeric_predictors(), threshold = -
      1))
    Condition
      Error in `step_pca()`:
      Caused by error in `prep()`:
      ! `threshold` must be a number between 0 and 1 or `NA`, not the number -1.

