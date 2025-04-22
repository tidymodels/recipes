# bad selector(s)

    Code
      step_count(rec, description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_count()`:
      x For this step, only a single selector can be used.
      i The following 2 selectors were used: `~description` and `~rows`.

---

    Code
      prep(rec2, training = covers)
    Condition
      Error in `step_count()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 1 integer variable found: `rows`

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_count()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `Sepal.Width`

# checks for grepl arguments

    Code
      step_count(recipe(~., data = mtcars), options = list(not_real_option = TRUE))
    Condition
      Error in `step_count()`:
      x The following elements of `options` are not allowed:
      * "not_real_option".
      i Valid options are: "ignore.case", "perl", "fixed", and "useBytes".

# check_options() is used

    Code
      prep(step_count(recipe(~description, data = covers), description, options = TRUE))
    Condition
      Error in `step_count()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = mt_tibble[, c(-1)])
    Condition
      Error in `step_count()`:
      ! The following required column is missing from `new_data`: make_model.

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
      * Regular expression counts using: <none>

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
      * Regular expression counts using: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_count()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Regular expression counts using: description

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 40 data points and no incomplete rows.
      
      -- Operations 
      * Regular expression counts using: description | Trained

# bad args

    Code
      prep(step_count(recipe(~description, covers), description, pattern = character(
        0)))
    Condition
      Error in `step_count()`:
      ! `pattern` must be a single string, not an empty character vector.

---

    Code
      prep(step_count(recipe(~description, covers), description, pattern = "(rock|stony)",
      result = letters))
    Condition
      Error in `step_count()`:
      Caused by error in `prep()`:
      ! `result` must be a single string, not a character vector.

---

    Code
      prep(step_count(recipe(~description, covers), description, pattern = "(rock|stony)",
      normalize = "yes"))
    Condition
      Error in `step_count()`:
      Caused by error in `prep()`:
      ! `normalize` must be `TRUE` or `FALSE`, not the string "yes".

