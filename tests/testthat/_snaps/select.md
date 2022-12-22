# quasiquotation

    Code
      prep(rec_1, training = iris_train)
    Condition
      Error in `step_select()`:
      Caused by error in `prep()`:
      ! Problem while evaluating `all_of(sepal_vars)`.
      Caused by error in `as_indices_impl()`:
      ! object 'sepal_vars' not found

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Variables selected: Species, starts_with("Sepal"), Petal.Width

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
      * Variables selected: Species, Sepal.Length, Sepal.Width, petal_width | Trained

