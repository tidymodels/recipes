# PLS-DA, dense loadings, multiple outcomes

    Code
      prep(rec)
    Condition
      Error in `prep()`:
      ! `step_pls()` only supports multivariate models for numeric outcomes.

# PLS-DA, sparse loadings, multiple outcomes

    Code
      prep(rec)
    Condition
      Error in `prep()`:
      ! `step_pls()` only supports multivariate models for numeric outcomes.

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
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Operations:
      
      PLS feature extraction with all_predictors()

---

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      PLS feature extraction with carbon, hydrogen, oxygen, nitrogen, sulfur [trained]

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
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      PLS feature extraction with <none>

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
      
      PLS feature extraction with <none> [trained]

