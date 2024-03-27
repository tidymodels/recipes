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

# nzv with case weights

    Code
      recipe(~., dat_caseweights_x2) %>% step_nzv(all_predictors(), freq_cut = exp_freq_cut_int) %>%
        prep()
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 50 data points and no incomplete rows.
      
      -- Operations 
      * Sparse, unbalanced variable filter removed: x4 | Trained, weighted

---

    Code
      recipe(~., dat_caseweights_y) %>% step_nzv(all_predictors(), freq_cut = exp_freq_cut_frag -
        1e-04) %>% prep()
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 50 data points and no incomplete rows.
      
      -- Operations 
      * Sparse, unbalanced variable filter removed: x3 and x4 | Trained, ignored
        weights

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
      * Sparse, unbalanced variable filter on: <none>

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
      * Sparse, unbalanced variable filter removed: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Operations 
      * Sparse, unbalanced variable filter on: x1, x2, x3, x4

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 50 data points and no incomplete rows.
      
      -- Operations 
      * Sparse, unbalanced variable filter removed: x3 and x4 | Trained

