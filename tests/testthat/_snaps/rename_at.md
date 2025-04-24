# mulitple functions

    Code
      prep(rec, training = slice(iris, 1:75))
    Condition
      Error in `step_rename_at()`:
      Caused by error in `dplyr::rename_at()`:
      ! `.funs` must contain one renaming function, not 2.

# no input

    Code
      bake(prep(step_rename_at(iris_rec), training = iris), new_data = NULL,
      composition = "data.frame")
    Condition
      Error in `step_rename_at()`:
      ! Argument `fn` must be specified.

---

    Code
      bake(prep(step_rename_at(iris_rec, fn = ":=O"), training = iris), new_data = NULL,
      composition = "data.frame")
    Condition
      Error in `step_rename_at()`:
      Caused by error in `get()`:
      ! object ':=O' of mode 'function' was not found

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
      * Variable renaming for: <none>

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
      * Variable renaming for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Variable renaming for: contains("Sepal")

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
      * Variable renaming for: Sepal.Length Sepal.Width | Trained

