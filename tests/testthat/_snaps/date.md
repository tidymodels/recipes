# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_date()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `Dan_year`

# errors on wrong values of features

    Code
      recipe(~ Dan + Stefan, examples) %>% step_date(all_predictors(), features = "yearly") %>%
        prep()
    Condition
      Error in `step_date()`:
      x Possible values of `features` are "year", "doy", "mday", "week", "decimal", "semester", "quarter", "dow", or "month".
      i Invalid values were: "yearly".

---

    Code
      recipe(~ Dan + Stefan, examples) %>% step_date(all_predictors(), features = c(
        "daily", "monthly", "yearly")) %>% prep()
    Condition
      Error in `step_date()`:
      x Possible values of `features` are "year", "doy", "mday", "week", "decimal", "semester", "quarter", "dow", or "month".
      i Invalid values were: "daily", "monthly", and "yearly".

---

    Code
      recipe(~ Dan + Stefan, examples) %>% step_date(all_predictors(), features = c(
        "daily", "month", "yearly")) %>% prep()
    Condition
      Error in `step_date()`:
      x Possible values of `features` are "year", "doy", "mday", "week", "decimal", "semester", "quarter", "dow", or "month".
      i Invalid values were: "daily" and "yearly".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(date_rec, new_data = examples[, 2, drop = FALSE])
    Condition
      Error in `step_date()`:
      ! The following required column is missing from `new_data`: Dan.

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
      * Date features from: <none>

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
      * Date features from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_date()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Date features from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 10 data points and no incomplete rows.
      
      -- Operations 
      * Date features from: Dan Stefan | Trained

# bad args

    Code
      date_rec <- recipe(~ Dan + Stefan, examples) %>% step_date(all_predictors(),
      abbr = "nope") %>% prep()
    Condition
      Error in `step_date()`:
      Caused by error in `prep()`:
      ! `abbr` must be `TRUE` or `FALSE`, not the string "nope".

---

    Code
      date_rec <- recipe(~ Dan + Stefan, examples) %>% step_date(all_predictors(),
      label = "no!") %>% prep()
    Condition
      Error in `step_date()`:
      Caused by error in `prep()`:
      ! `label` must be `TRUE` or `FALSE`, not the string "no!".

---

    Code
      date_rec <- recipe(~ Dan + Stefan, examples) %>% step_date(all_predictors(),
      ordinal = "never") %>% prep()
    Condition
      Error in `step_date()`:
      Caused by error in `prep()`:
      ! `ordinal` must be `TRUE` or `FALSE`, not the string "never".

