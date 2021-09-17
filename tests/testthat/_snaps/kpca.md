# correct kernel PCA values

    Code
      kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6, id = "")

# printing

    Code
      kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6)

# No kPCA comps

    Code
      pca_extract <- rec %>% step_kpca(X2, X3, X4, X5, X6, num_comp = 0, id = "") %>%
        prep()

