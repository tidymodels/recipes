# altered freq_cut and unique_cut

    The `options` argument of `step_nzv()` was deprecated in recipes 0.1.7 and is now defunct.
    i Please use the arguments `freq_cut` and `unique_cut` instead.

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_nzv(options = list(freq_cut = 95 / 5,
      unique_cut = 20))
    Condition
      Error:
      ! The `options` argument of `step_nzv()` was deprecated in recipes 0.1.7 and is now defunct.
      i Please use the arguments `freq_cut` and `unique_cut` instead.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Operations:
      
      Sparse, unbalanced variable filter on x1, x2, x3, x4

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Training data contained 50 data points and no missing data.
      
      Operations:
      
      Sparse, unbalanced variable filter removed x3, x4 [trained]

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
      
      Sparse, unbalanced variable filter on <none>

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
      
      Sparse, unbalanced variable filter removed <none> [trained]

# nzv with case weights

    Code
      recipe(~., dat_caseweights_x2) %>% step_nzv(all_predictors(), freq_cut = exp_freq_cut_int) %>%
        prep()
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          4
      
      Training data contained 50 data points and no missing data.
      
      Operations:
      
      Sparse, unbalanced variable filter removed x4 [weighted, trained]

---

    Code
      recipe(~., dat_caseweights_y) %>% step_nzv(all_predictors(), freq_cut = exp_freq_cut_frag -
        1e-04) %>% prep()
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          4
      
      Training data contained 50 data points and no missing data.
      
      Operations:
      
      Sparse, unbalanced variable filter removed x3, x4 [ignored weights, trained]

