# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_nnmf_sparse()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `NNMF1`

# rethrows error correctly from implementation

    Code
      recipe(~., data = mtcars) %>% step_nnmf_sparse(all_predictors()) %>% prep()
    Condition
      Error in `step_nnmf_sparse()`:
      Caused by error in `prep()`:
      x Failed with error:
      i Error in RcppML::nmf(A = dat, k = 2, L1 = c(0.001, 0.001), verbose = FALSE, : mocked error

# errors for missing data

    Code
      recipe(~., data = mtcars) %>% step_nnmf_sparse(all_predictors()) %>% prep()
    Condition
      Error in `step_nnmf_sparse()`:
      Caused by error in `prep()`:
      x The NNMF loadings are missing.
      i The penalty may have been too high or missing values are present in data.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mtcars[, -3])
    Condition
      Error in `step_nnmf_sparse()`:
      ! The following required column is missing from `new_data`: disp.

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
      * Non-negative matrix factorization for: <none>

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
      * No non-negative matrix factorization was extracted from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_nnmf_sparse()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Non-negative matrix factorization for: disp and drat

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Non-negative matrix factorization for: disp and drat | Trained

# bad args

    Code
      recipe(mpg ~ ., mtcars) %>% step_nnmf_sparse(disp, drat, num_comp = -1) %>%
        prep()
    Condition
      Error in `step_nnmf_sparse()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      recipe(mpg ~ ., mtcars) %>% step_nnmf_sparse(disp, drat, penalty = -1) %>% prep()
    Condition
      Error in `step_nnmf_sparse()`:
      Caused by error in `prep()`:
      ! `penalty` must be a number larger than or equal to 2.22044604925031e-16, not the number -1.

---

    Code
      recipe(mpg ~ ., mtcars) %>% step_nnmf_sparse(disp, drat, prefix = 1) %>% prep()
    Condition
      Error in `step_nnmf_sparse()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 1.

