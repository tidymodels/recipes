# error when neither sep or pattern is specified

    Code
      prep(step_dummy_extract(recipe(~medium, data = tate_text), medium))
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `prep()`:
      ! `sep` or `pattern` must be specified.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `Species_setosa`

# case weights

    Code
      dummy_prepped
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Extract patterns from: medium | Trained, weighted

---

    Code
      dummy_prepped
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Extract patterns from: medium | Trained, ignored weights

# bake method errors when needed non-standard role columns are missing

    Code
      bake(dummy_prepped, new_data = mini_tate[, 1:3])
    Condition
      Error in `step_dummy_extract()`:
      ! The following required column is missing from `new_data`: medium.

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
      * Extract patterns from: <none>

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
      * Extract patterns from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_dummy_extract()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Extract patterns from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4284 data points and no incomplete rows.
      
      -- Operations 
      * Extract patterns from: medium | Trained

# bad args

    Code
      prep(step_dummy_extract(recipe(~colors, data = color_examples), colors,
      pattern = "(?<=')[^',]+(?=')", other = 2))
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `prep()`:
      ! `other` must be a single string or `NULL`, not the number 2.

---

    Code
      prep(step_dummy_extract(recipe(~colors, data = color_examples), colors,
      pattern = "(?<=')[^',]+(?=')", other = 2))
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `prep()`:
      ! `other` must be a single string or `NULL`, not the number 2.

---

    Code
      prep(step_dummy_extract(recipe(~colors, data = color_examples), colors,
      pattern = "(?<=')[^',]+(?=')", sep = 2))
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `prep()`:
      ! `sep` must be a single string or `NULL`, not the number 2.

---

    Code
      prep(step_dummy_extract(recipe(~colors, data = color_examples), colors,
      pattern = 2))
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `prep()`:
      ! `pattern` must be a single string or `NULL`, not the number 2.

---

    Code
      prep(step_dummy_extract(recipe(~colors, data = color_examples), colors,
      pattern = "(?<=')[^',]+(?=')", naming = NULL))
    Condition
      Error in `step_dummy_extract()`:
      Caused by error in `prep()`:
      ! `naming` must be a function, not `NULL`.

