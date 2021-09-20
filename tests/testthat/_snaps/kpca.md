# printing

    Code
      kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6)

---

    Code
      kpca_rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Operations:
      
      Kernel PCA extraction with X2, X3, X4, X5, X6

---

    Code
      prep(kpca_rec, training = tr_dat, verbose = TRUE)
    Output
      oper 1 step kpca [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Kernel PCA (rbfdot) extraction with X2, X3, X4, X5, X6 [trained]

# No kPCA comps

    Code
      pca_extract <- rec %>% step_kpca(X2, X3, X4, X5, X6, num_comp = 0, id = "") %>%
        prep()

---

    Code
      pca_extract
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      No kPCA components were extracted.
       [trained]

# can prep recipes with no keep_original_cols

    Code
      kpca_trained <- prep(kpca_rec, training = tr_dat, verbose = FALSE)
    Warning <warning>
      'keep_original_cols' was added to `step_kpca()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Kernel PCA extraction with 

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      No kPCA components were extracted.
       [trained]

