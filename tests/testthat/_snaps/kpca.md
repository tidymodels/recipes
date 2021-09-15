# correct kernel PCA values

    Code
      kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6, id = "")
    Message <message>
      `step_kpca()` is deprecated in favor of either `step_kpca_rbf()` or `step_kpca_poly()`. It will be removed in future versions.

# printing

    Code
      kpca_rec <- rec %>% step_kpca(X2, X3, X4, X5, X6)
    Message <message>
      `step_kpca()` is deprecated in favor of either `step_kpca_rbf()` or `step_kpca_poly()`. It will be removed in future versions.

# No kPCA comps

    Code
      pca_extract <- rec %>% step_kpca(X2, X3, X4, X5, X6, num_comp = 0, id = "") %>%
        prep()
    Message <message>
      `step_kpca()` is deprecated in favor of either `step_kpca_rbf()` or `step_kpca_poly()`. It will be removed in future versions.

