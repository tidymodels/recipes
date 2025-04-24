# bad input

    Code
      prep(step_sample(iris_rec, size = -1))
    Condition
      Error in `step_sample()`:
      Caused by error in `prep()`:
      ! `size` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      prep(step_sample(iris_rec, size = "a"))
    Condition
      Error in `step_sample()`:
      Caused by error in `prep()`:
      ! `size` must be a number or `NULL`, not the string "a".

---

    Code
      prep(step_sample(iris_rec, replace = "a"))
    Condition
      Error in `step_sample()`:
      Caused by error in `prep()`:
      ! `replace` must be `TRUE` or `FALSE`, not the string "a".

# sample with case weights

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    10
      case_weights:  1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Row sampling: <none> | Trained, weighted

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    10
      case_weights:  1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Row sampling: <none> | Trained, weighted

# warn when selectors are provided

    Code
      tmp <- step_sample(recipe(~., data = mtcars), all_predictors())
    Condition
      Warning:
      Selectors are not used for this step.

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
      * Row sampling: <none>

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
      * Row sampling: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Row sampling: <none>

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Row sampling: <none> | Trained

