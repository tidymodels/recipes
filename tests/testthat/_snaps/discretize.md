# bad values

    Code
      discretize(letters)
    Condition
      Error in `discretize()`:
      ! Only numeric `x` is accepted

# printing of discretize()

    Code
      discretize(1:100)
    Output
      Bins: 5 (includes missing category)
      Breaks: -Inf, 25.75, 50.5, 75.25, Inf

---

    Code
      discretize(1:100, cuts = 6)
    Output
      Bins: 7 (includes missing category)

---

    Code
      discretize(1:100, keep_na = FALSE)
    Output
      Bins: 4
      Breaks: -Inf, 25.75, 50.5, 75.25, Inf

---

    Code
      res <- discretize(1:2)
    Condition
      Warning:
      Data not binned; too few unique values per bin. Adjust 'min_unique' as needed

---

    Code
      res
    Output
      Too few unique data points. No binning was used.

# multiple column prefix

    Code
      recipe(~., data = example_data) %>% step_discretize(x1, x2, options = list(
        prefix = "hello")) %>% prep()
    Condition
      Warning:
      Note that the options `prefix` and `labels` will be applied to all variables
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 1000 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1, x2 | Trained

# bad args

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, num_breaks = 1) %>% prep()
    Condition
      Error in `step_discretize()`:
      Caused by error in `recipes::discretize()`:
      ! There should be at least 2 cuts

---

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, num_breaks = 100) %>% prep()
    Condition
      Warning:
      Data not binned; too few unique values per bin. Adjust 'min_unique' as needed
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1 | Trained

---

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, options = list(prefix = "@$")) %>%
        prep()
    Condition
      Warning:
      The prefix '@$' is not a valid R name. It has been changed to 'X..'.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1 | Trained

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
      * Discretize numeric variables from: <none>

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
      * Discretize numeric variables from: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Discretize numeric variables from: x1

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1 | Trained

