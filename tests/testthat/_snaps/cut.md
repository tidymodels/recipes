# step_cut throws error on non-numerics

    Code
      recipe(x) %>% step_cut(cat_var, breaks = 2) %>% prep()
    Condition
      Error in `step_cut()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 string variable found: `cat_var`

---

    Code
      recipe(~., x) %>% step_cut(all_predictors(), breaks = 2) %>% prep()
    Condition
      Error in `step_cut()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `cat_var`

---

    Code
      recipe(~., x) %>% step_cut(num_var, breaks = 2, include_outside_range = 2) %>%
        prep()
    Condition
      Error in `step_cut()`:
      Caused by error in `prep()`:
      ! `include_outside_range` must be `TRUE` or `FALSE`, not the number 2.

# full_breaks_check will give warnings

    Code
      full_breaks_check(10)
    Condition
      Error:
      ! Variable is invariant and equal to break point.

---

    Code
      full_breaks_check(c(10, 20))
    Condition
      Warning:
      This will create a factor with one value only.

# breaks argument are type checked

    Code
      recipe(~., data = mtcars) %>% step_cut(disp, hp, breaks = TRUE) %>% prep()
    Condition
      Error in `step_cut()`:
      Caused by error in `prep()`:
      ! `breaks` must be a numeric vector, not `TRUE`.

---

    Code
      recipe(~., data = mtcars) %>% step_cut(disp, hp, breaks = c("100", "200")) %>%
        prep()
    Condition
      Error in `step_cut()`:
      Caused by error in `prep()`:
      ! `breaks` must be a numeric vector, not a character vector.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(prepped, df[, 2, drop = FALSE])
    Condition
      Error in `step_cut()`:
      ! The following required column is missing from `new_data`: x.

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
      * Cut numeric for: <none>

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
      * Cut numeric for: <none> | Trained

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
      * Cut numeric for: disp

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
      * Cut numeric for: disp | Trained

