# printing

    Code
      kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6)

---

    Code
      kpca_rec
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
      prep(kpca_rec)
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

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_kpca()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  kPC1

# No kPCA comps

    Code
      pca_extract <- rec %>% step_kpca(X2, X3, X4, X5, X6, num_comp = 0, id = "") %>%
        prep()

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

# can prep recipes with no keep_original_cols

    Code
      kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_kpca()` after this recipe was created.
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

