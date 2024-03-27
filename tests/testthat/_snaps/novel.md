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

