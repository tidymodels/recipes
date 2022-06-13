# correct PCA values

    Code
      tidy(pca_extract_trained, number = 3, type = "variances")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "coef", "variance"

# printing

    Code
      print(pca_extract)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Operations:
      
      PCA extraction with carbon, hydrogen, oxygen, nitrogen, sulfur

---

    Code
      prep(pca_extract)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      PCA extraction with carbon, hydrogen, oxygen, nitrogen, sulfur [trained]

# No PCA comps

    Code
      print(pca_extract_trained)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      No PCA components were extracted from carbon, hydrogen, oxygen, nitrogen, sulfur [trained]

# backwards compatible with 0.1.17

    Code
      pca_extract
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Centering for carbon, hydrogen, oxygen, nitrogen, sulfur [trained]
      Scaling for carbon, hydrogen, oxygen, nitrogen, sulfur [trained]
      PCA extraction with carbon, hydrogen, oxygen, nitrogen, sulfur [trained]

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
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      PCA extraction with <none>

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
      
      No PCA components were extracted from <none> [trained]

# case weights

    Code
      pca_extract_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          4
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      PCA extraction with carbon, hydrogen, oxygen, sulfur [weighted, trained]

---

    Code
      pca_extract_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          4
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      PCA extraction with carbon, hydrogen, oxygen, sulfur [ignored weights, trained]

