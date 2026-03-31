# step_nnmf() is deprecated

    Code
      suppressMessages(step_nnmf(recipe(~., data = mtcars), all_numeric_predictors()))
    Condition
      Warning:
      `step_nnmf()` was deprecated in recipes 0.2.0.
      i Please use `step_nnmf_sparse()` instead.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Operations 
      * Non-negative matrix factorization for: all_numeric_predictors()

