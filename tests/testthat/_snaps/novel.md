# bad args

    Code
      recipe(~., data = iris) %>% step_novel(all_predictors()) %>% prep(iris)
    Condition
      Error in `step_novel()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 4 double variables found: `Sepal.Length`, `Sepal.Width`, ...

---

    Code
      recipe(~., data = tr_bad) %>% step_novel(all_predictors()) %>% prep(tr_bad)
    Condition
      Error in `step_novel()`:
      Caused by error in `prep()`:
      ! Columns already contain the new level: x.

---

    Code
      rec %>% step_novel(all_predictors(), new_level = letters) %>% prep()
    Condition
      Error in `step_novel()`:
      Caused by error in `prep()`:
      ! `new_level` must be a single string, not a character vector.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(ex_1, new_data = tr_dat[, c(-3)])
    Condition
      Error in `step_novel()`:
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
      * Novel factor level assignment for: <none>

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
      * Novel factor level assignment for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Novel factor level assignment for: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 200 data points and no incomplete rows.
      
      -- Operations 
      * Novel factor level assignment for: v, w, x, y, z | Trained

