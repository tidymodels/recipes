# No kPCA comps

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
      * RBF kernel PCA extraction with: X2, X3, X4, X5, X6 | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_kpca_rbf()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `kPC1`

# rethrows error correctly from implementation

    Code
      recipe(~., data = mtcars) %>% step_kpca_rbf(all_predictors()) %>% prep()
    Condition
      Error in `step_kpca_rbf()`:
      Caused by error in `prep()`:
      x Failed with error:
      i Error in kernlab::kpca(x = as.matrix(training[, col_names]), features = 5, : mocked error

# bake method errors when needed non-standard role columns are missing

    Code
      bake(kpca_trained, new_data = te_dat[, 1:3])
    Condition
      Error in `step_kpca_rbf()`:
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
      * RBF kernel PCA extraction with: <none>

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
      * RBF kernel PCA extraction with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_kpca_rbf()` after this recipe was created.
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

# bad args

    Code
      recipe(~., data = tr_dat) %>% step_kpca_rbf(all_numeric_predictors(), num_comp = -
      1) %>% prep()
    Condition
      Error in `step_kpca_rbf()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      recipe(~., data = tr_dat) %>% step_kpca_rbf(all_numeric_predictors(), sigma = 0) %>%
        prep()
    Condition
      Error in `step_kpca_rbf()`:
      Caused by error in `prep()`:
      ! `sigma` must be a number larger than or equal to 2.22044604925031e-16, not the number 0.

---

    Code
      recipe(~., data = tr_dat) %>% step_kpca_rbf(all_numeric_predictors(), prefix = 1) %>%
        prep()
    Condition
      Error in `step_kpca_rbf()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 1.

