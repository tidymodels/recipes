# bad selector(s)

    Code
      step_regex(rec, description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_regex()`:
      x For this step, only a single selector can be used.
      i The following 2 selectors were used: `~description` and `~rows`.

---

    Code
      prep(rec4, training = covers)
    Condition
      Error in `step_regex()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 1 integer variable found: `rows`

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_regex()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `Sepal.Width`

# error on multiple selections

    Code
      step_regex(recipe(~., data = mtcars), vs, am)
    Condition
      Error in `step_regex()`:
      x For this step, only a single selector can be used.
      i The following 2 selectors were used: `~vs` and `~am`.

# checks for grepl arguments

    Code
      step_regex(recipe(~., data = mtcars), options = list(not_real_option = TRUE))
    Condition
      Error in `step_regex()`:
      x The following elements of `options` are not allowed:
      * "not_real_option".
      i Valid options are: "ignore.case", "perl", "fixed", and "useBytes".

# check_options() is used

    Code
      prep(step_regex(recipe(~Species, data = iris), Species, options = TRUE))
    Condition
      Error in `step_regex()`:
      Caused by error in `prep()`:
      ! `options` must be a list, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = mt_tibble[, c(-1)])
    Condition
      Error in `step_regex()`:
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
      * Regular expression dummy variable using: "."

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
      * Regular expression dummy variable using: "." | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_regex()` after this recipe was created.
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
      * Regular expression dummy variable using: "(rock|stony)"

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
      * Regular expression dummy variable using: "(rock|stony)" | Trained

# bad args

    Code
      prep(step_regex(rec, description, pattern = character(0)))
    Condition
      Error in `step_regex()`:
      Caused by error in `prep()`:
      ! `pattern` must be a single string, not an empty character vector.

