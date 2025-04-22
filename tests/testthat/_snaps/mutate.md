# quasiquotation

    Code
      prep(rec_1, training = slice(iris, 1:75))
    Condition
      Error in `step_mutate()`:
      Caused by error in `dplyr::mutate()`:
      i In argument: `new_var = Sepal.Width * const`.
      Caused by error:
      ! object 'const' not found

# required_pkgs.step_mutate() works

    Code
      step_mutate(recipe(~., data = mtcars), new = 2, .pkgs = "not-a-package")
    Message
      1 package (not-a-package) is needed for this step but is not installed.
      To install run: `install.packages("not-a-package")`
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Operations 
      * Variable mutation for: 2

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
      * Variable mutation for: <none>

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
      * Variable mutation for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Variable mutation for: 5

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
      * Variable mutation for: ~5 | Trained

