# altered freq_cut and unique_cut

    The `options` argument of `step_nzv()` was deprecated in recipes 0.1.7 and is now defunct.
    i Please use the arguments `freq_cut` and `unique_cut` instead.

# Deprecation warning

    Code
      step_nzv(recipe(~., data = mtcars), options = list(freq_cut = 95 / 5,
      unique_cut = 20))
    Condition
      Error:
      ! The `options` argument of `step_nzv()` was deprecated in recipes 0.1.7 and is now defunct.
      i Please use the arguments `freq_cut` and `unique_cut` instead.

# nzv with case weights

    Code
      prep(step_nzv(recipe(~., dat_caseweights_x2), all_predictors(), freq_cut = exp_freq_cut_int))
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
      prep(step_nzv(recipe(~., dat_caseweights_y), all_predictors(), freq_cut = exp_freq_cut_frag -
        1e-04))
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 50 data points and no incomplete rows.
      
      -- Operations 
      * Sparse, unbalanced variable filter removed: x3 x4 | Trained, ignored weights

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
      * Sparse, unbalanced variable filter removed: x3 x4 | Trained

# bad args

    Code
      prep(step_nzv(recipe(y ~ ., data = dat), x1, freq_cut = -1))
    Condition
      Error in `step_nzv()`:
      Caused by error in `prep()`:
      ! `freq_cut` must be a number larger than or equal to 0, not the number -1.

---

    Code
      prep(step_nzv(recipe(y ~ ., data = dat), x1, unique_cut = 101))
    Condition
      Error in `step_nzv()`:
      Caused by error in `prep()`:
      ! `unique_cut` must be a number between 0 and 100, not the number 101.

