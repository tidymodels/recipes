# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_kpca()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `kPC1`

# No kPCA comps

    Code
      pca_extract <- prep(step_kpca(rec, X2, X3, X4, X5, X6, num_comp = 0, id = ""))

---

    Code
      pca_extract
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Kernel PCA extraction with: X2, X3, X4, X5, X6 | Trained

# rethrows error correctly from implementation

    Code
      prep(step_kpca(recipe(~., data = mtcars), all_predictors()))
    Condition
      Error in `step_kpca()`:
      Caused by error in `prep()`:
      ! Failed to compute:
      Caused by error in `kernlab::kpca()`:
      ! mocked error

# check_options() is used

    Code
      prep(step_kpca(recipe(~mpg, data = mtcars), mpg, options = TRUE))
    Condition
      Error in `step_kpca()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(kpca_trained, new_data = te_dat[, 1:3])
    Condition
      Error in `step_kpca()`:
      ! The following required columns are missing from `new_data`: X4, X5, and X6.

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
      * Kernel PCA extraction with: <none>

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
      * Kernel PCA extraction with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_kpca()` after this recipe was created.
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
      * Kernel PCA extraction with: X2, X3, X4, X5, X6

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
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Kernel PCA extraction with: X2, X3, X4, X5, X6 | Trained

# bad args

    Code
      prep(step_kpca(recipe(~., data = tr_dat), all_numeric_predictors(), num_comp = -
      1))
    Condition
      Error in `step_kpca()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      prep(step_kpca(recipe(~., data = tr_dat), all_numeric_predictors(), prefix = 1))
    Condition
      Error in `step_kpca()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 1.

