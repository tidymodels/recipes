# PLS-DA, dense loadings, multiple outcomes

    Code
      prep(rec)
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! Only multivariate models for numeric outcomes are supports.

# PLS-DA, sparse loadings, multiple outcomes

    Code
      prep(rec)
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! Only multivariate models for numeric outcomes are supports.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pls()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `PLS1`

# Deprecation warning

    Code
      recipe(~., data = mtcars) %>% step_pls(outcome = mpg, preserve = TRUE)
    Condition
      Error:
      ! The `preserve` argument of `step_pls()` was deprecated in recipes 0.1.16 and is now defunct.
      i Please use the `keep_original_cols` argument instead.

# rethrows error correctly from implementation

    Code
      tmp <- recipe(~., data = mtcars) %>% step_pls(all_predictors(), outcome = mpg) %>%
        prep()
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! Failed to compute:
      Caused by error in `mixOmics::pls()`:
      ! mocked error

# error on no outcome

    Code
      recipe(~., data = mtcars) %>% step_pls(all_predictors()) %>% prep()
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `outcome` must not be `NULL`.

# check_options() is used

    Code
      recipe(~., data = mtcars) %>% step_pls(disp, outcome = mpg, options = TRUE) %>%
        prep()
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# recipes_argument_select() is used

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_pls(disp, outcome = NULL) %>% prep()
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `outcome` must not be `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = biom_tr[, c(-1)])
    Condition
      Error in `step_pls()`:
      ! The following required column is missing from `new_data`: carbon.

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
      * PLS feature extraction with: <none>

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
      * PLS feature extraction with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_pls()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * PLS feature extraction with: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * PLS feature extraction with: carbon, hydrogen, oxygen, ... | Trained

# bad args

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_pls(-mpg, outcome = mpg, num_comp = -1) %>%
        prep()
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_pls(-mpg, outcome = mpg, prefix = 1) %>%
        prep()
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not the number 1.

---

    Code
      recipe(mpg ~ ., data = mtcars) %>% step_pls(-mpg, outcome = mpg,
        predictor_prop = -1) %>% prep()
    Condition
      Error in `step_pls()`:
      Caused by error in `prep()`:
      ! `predictor_prop` must be a number between 0 and 1, not the number -1.

