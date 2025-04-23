# dummy variables with non-factor inputs

    Code
      prep(dummy)
    Condition
      Error in `step_dummy_multi_choice()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be nominal or logical.
      * 11 double variables found: `mpg`, `cyl`, `disp`, `hp`, ...

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_dummy_multi_choice()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `Species_setosa`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = languages[, -1])
    Condition
      Error in `step_dummy_multi_choice()`:
      ! The following required column is missing from `new_data`: lang_1.

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
      * Multi-choice dummy variables from: <none>

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
      * Multi-choice dummy variables from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_dummy_multi_choice()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Multi-choice dummy variables from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 4 data points and 4 incomplete rows.
      
      -- Operations 
      * Multi-choice dummy variables from: lang_1, lang_2, lang_3, ... | Trained

# bad args

    Code
      dummy_multi_choice_rec <- prep(step_dummy_multi_choice(recipe(~., data = languages),
      starts_with("lang"), other = 2))
    Condition
      Error in `step_dummy_multi_choice()`:
      Caused by error in `prep()`:
      ! `other` must be a single string or `NULL`, not the number 2.

---

    Code
      dummy_multi_choice_rec <- prep(step_dummy_multi_choice(recipe(~., data = languages),
      starts_with("lang"), naming = NULL))
    Condition
      Error in `step_dummy_multi_choice()`:
      Caused by error in `prep()`:
      ! `naming` must be a function, not `NULL`.

